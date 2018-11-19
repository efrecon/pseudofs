package require Tcl 8.6

namespace eval ::procfs {
    namespace eval vars {
        variable -proc   "/proc"
        variable -hook   {}
        variable CLK_TCK -1
    }
    namespace export {[a-z]*}
    namespace ensemble create
}


proc ::procfs::configure { args } {
    if { [llength $args] == 0 } {
        set cfg [dict create]
        foreach v [info vars vars::-*] {
            dict set cfg [lindex [split $v ":"] end] [set $v]
        }
        return $cfg
    } elseif { [llength $args] == 1 } {
        set v -[string trimleft [lindex $args 0] -]
        if { [info exists vars::$v] } {
            return [set vars::$v]
        } else {
            return ""
        }
    } else {
        foreach {k v} $args {
            set k -[string trimleft $k -]
            if { [info exists vars::$k] } {
                set vars::$k $v
            } else {
                return -code error "$k is an unknown configuration option, should be one of [join [dict keys [configure]] , ]"
            }
        }
    }
}

# ::procfs::pids -- List of processes
#
#      Return the list of processes running at the caller at this point in time.
#
# Arguments:
#      None.
#
# Results:
#      List of process identifiers.
#
# Side Effects:
#      None.
proc ::procfs::pids {} {
    set pids [list]
    foreach dir [Glob ${vars::-proc} * -types d -tails -nocomplain] {
        if { [string is integer -strict $dir] } {
            lappend pids $dir
        }
    }
    return $pids
}


# ::procfs::cmdline -- Process command-line
#
#      Returns a well-formatted list representing entire command-line and
#      arguments for a given process.
#
# Arguments:
#      pid      Process identifier
#
# Results:
#      A list with the process and its arguments.
#
# Side Effects:
#      None.
proc ::procfs::cmdline { pid } {
    return [GetFileContent \
                [file join ${vars::-proc} $pid cmdline] \
                {cmdline {return [split [string trim $cmdline "\0"] "\0"]}}]
}


# ::procfs::comm -- Process command
#
#      Return the name of the command that started a given process.
#
# Arguments:
#      pid      Process identifier
#
# Results:
#      Return the name of the (binary) for a given process. No arguments.
#
# Side Effects:
#      None.
proc ::procfs::comm { pid } {
    return [GetFileContent \
                [file join ${vars::-proc} $pid comm] \
                { c {return [string trim $c]}}]
}


# ::procfs::io -- Disk I/O
#
#      Returns a dictionary representing the Disk I/O for a given process.  The
#      dictionary will contain the following keys: rchar, wchar, syscr, syscw,
#      read_bytes, write_bytes, cancel_write_bytes. Consult the man page for
#      more information
#
# Arguments:
#      pid      Process identifier
#
# Results:
#      A well-formed dictionary.
#
# Side Effects:
#      None.
proc ::procfs::io { pid } {
    return [Colon2Dict [file join ${vars::-proc} $pid io]]
}


# ::procfs::interfaces -- List network interfaces
#
#      List the network interfaces available to a process or to the entire
#      system.
#
# Arguments:
#      pid      Process identifier (-1 for system-wide interface list)
#
# Results:
#      List of the network interface names
#
# Side Effects:
#      None.
proc ::procfs::interfaces { { pid -1 } }  {
    if { $pid >= 0 } {
        set fpath [file join ${vars::-proc} $pid net dev]
    } else {
        set fpath [file join ${vars::-proc} net dev]
    }
    return [Lines $fpath \
                {line {
                    set idx [string first ":" $line]
                    if { $idx >= 0 } {
                        return [string range $line 0 [expr {$idx-1}]]
                    }
                }} -trim -skip]
}


# ::procfs::netstats -- Process/System network stats
#
#      Return a dictionary with network statistics for the whole system or a
#      given process, for one or some of its interfaces. When requesting for
#      receive stats, the dictionary will have the following keys: bytes,
#      packets, errs, drop, fifo, frame, compressed, multicast. When requesting
#      for transmit stats, the dictionary will have the following keys:
#      bytes,packets, errs, drop, fifo, colls, carrier, compressed. The name of
#      these keys is as of the content of the orginal file and their meaning is
#      as of the proc manual page. When selecting several interfaces, the keys
#      will contain cumulated data.
#
# Arguments:
#      pid       Process identifier (negative for system-wide stats)
#      direction One of R or T (for receive or transmit)
#      interface Glob-style pattern matching the interface name (defaults to *)
#
# Results:
#      A dictionary containing the (cumulated) statistics for the matching
#      interfaces.
#
# Side Effects:
#      None.
proc ::procfs::netstats { { pid -1 } { direction "R" } { interface "*"} } {
    # Allow calling this with "receive" or "RECEIVE" or whatever starting with R
    # for receive and T for transmit.
    set direction [string index [string toupper $direction] 0]
    if { $direction ni [list "R" "T"]} {
        error "Direction is not recognised! Should start with R (Receive) or T (Transmit)"
    }

    set d {}
    set locators {}
    dict set fields R {}
    dict set fields T {}
    set stats {}
    if { $pid >= 0 } {
        set fpath [file join ${vars::-proc} $pid net dev]
    } else {
        set fpath [file join ${vars::-proc} net dev]    
    }
    foreach line [Lines $fpath {} -trim -skip] {
        # Below is a typical dev content file that the code below is able to
        # parse. The idea is to parse the headers to first locate using the
        # first line where receive and transmit are, then discover the
        # fields thar will be present in receive and transmit, and then
        # accumulate for each network interface matching the pattern.
        #
        #  Inter-|   Receive                                                |  Transmit
        #   face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed
        #      lo: 2776770   11307    0    0    0     0          0         0  2776770   11307    0    0    0     0       0          0
        #    eth0: 1215645    2751    0    0    0     0          0         0  1782404    4324    0    0    0   427       0          0
        #    ppp0: 1622270    5552    1    0    0     0          0         0   354130    5669    0    0    0     0       0          0
        #    tap0:    7714      81    0    0    0     0          0         0     7714      81    0    0    0     0       0          0
        if { [dict size $locators] == 0 } {
            # First line of header is where the receiver and transmit
            # information is located (note that some information about the
            # interface preceeds this)
            set i 0
            foreach f [split $line |] {
                set f [string trim $f]
                if { [string match -nocase "R*" $f] } {
                    dict set locators "R" $i
                }
                if { [string match -nocase "T*" $f] } {
                    dict set locators "T" $i
                }
                incr i
            }
        } elseif {[llength [dict get $fields "R"]] == 0 } {
            # Second line of headers are the various fields for each IO
            # direction.
            set headers [split $line |]
            dict set fields R [lindex $headers [dict get $locators "R"]]
            dict set fields T [lindex $headers [dict get $locators "T"]]
        } else {
            # All other lines are data about interfaces. First come the
            # interface name, then the data with Receiver and Transmit
            # fields (or the opposite), which we segregate properly using
            # the detection above.
            set idx [string first ":" $line]
            if { $idx >= 0 } {
                # Interface first
                set iface [string trim [string range $line 0 [expr {$idx-1}]]]
                # Data for the interface, fields for receiver then transmit,
                # or the opposite. There isn't any separator between R and
                # T, so we use the fields detected above.
                set vals [string trim [string range $line [expr {$idx+1}] end]]
                # Only select interface that match the pattern.
                if { [string match $interface $iface] } {
                    # Detect which is first and extract into variables.
                    # These variables match the value of direction, we have
                    # made sure that this is the case at the very beginning
                    # of the proc.
                    if { [dict get $locators "R"] < [dict get $locators "T"] } {
                        set R [lrange $vals 0 [expr {[llength [dict get $fields "R"]]-1}]]
                        set T [lrange $vals [llength [dict get $fields "R"]] end]
                    } else {
                        set T [lrange $vals 0 [expr {[llength [dict get $fields "T"]]-1}]]
                        set R [lrange $vals [llength [dict get $fields "T"]] end]
                    }

                    # Cumulate statistics into the stats dictionary.
                    for {set i 0} {$i<[llength [dict get $fields $direction]]} {incr i} {
                        if { [dict exists $stats [lindex [dict get $fields $direction] $i]] } {
                            dict incr stats [lindex [dict get $fields $direction] $i] [lindex [set $direction] $i] 
                        } else {
                            dict set stats [lindex [dict get $fields $direction] $i] [lindex [set $direction] $i] 
                        }
                    } 
                }
            }
        }
    }

    return $stats
}


# ::procfs::stat -- Process/system stat
#
#      Return a dictionary representing the process or system stat. The keys
#      contained in these dictionaries will vary considerably when requesting
#      stat for a process or the entire system.  For processes, these are as
#      described in the /proc/[pid]/stat manual page. For the system, the keys
#      will be cpu, cpu0, cpu1, .., page, swap, intr, ctxt, btime, processes,
#      procs_running, procs_blocked. Each cpu related key will point to another
#      dictionary with the keys: user, nice, system, idle, iowait, irq softirq,
#      steal, guest, guest_nice. For page and swap, this will be another
#      dictionary with the keys in and out.  Unless specially requested, all
#      time-related values are automatically converted to fractions of seconds.
#
# Arguments:
#      pid      Process identifier (negative for system-wide stats)
#      convert  Convert from CLK_TCK (defaults to yes)
#
# Results:
#      A dictionary as described above.
#
# Side Effects:
#      None.
proc ::procfs::stat { { pid -1 } { convert false } } {
    if { $pid >= 0 } {
        return [StatPID $pid $convert]
    }
    set stat {}
    foreach line [Lines [file join ${vars::-proc} stat] {} -trim -skip] {
        switch -glob -- [lindex $line 0] {
            "cpu*" {
                # Support for newer vars on lower versions of the kernel
                lappend line 0 0 0 0 0 0 0 0 0 0
                lassign $line key user nice system idle iowait irq softirq steal guest guest_nice
                set cpu [dict create \
                            user $user \
                            nice $nice \
                            system $system \
                            idle $idle \
                            iowait $iowait \
                            irq $irq \
                            softirq $softirq \
                            steal $steal \
                            guest $guest \
                            guest_nice $guest_nice]
                if { $convert } {
                    foreach tm [list user nice system idle iowait irq softirq steal guest guest_nice] {
                        dict set cpu $tm [expr {[dict get $cpu $tm]/[CLK_TCK]}]
                    }
                }
                dict set stat $key $cpu
            }
            "page" -
            "swap" {
                lassign $line key in out
                set io [dict create in $in out $out]
                dict set stat $key $io
            }
            "intr" -
            "ctxt" -
            "btime" -
            "processes" -
            "procs_running" -
            "procs_blocked" {
                lassign $line key val
                dict set stat $key $val
            }
        }
        
    }
    return $stat
}


# ::procfs::statm -- Memory statistics
#
#      Returns a dictionary with memory statistics for a given process. The keys
#      of the dictionary are size, resident, shared, text, lib, data and dt.
#      They are described in the /proc/[pid]/statm section of the manual.
#
# Arguments:
#      pid      Process identifier
#
# Results:
#      Dictionary with memory statistics for process.
#
# Side Effects:
#      None.
proc ::procfs::statm { pid } {
    set line [GetFileContent \
                [file join ${vars::-proc} $pid statm] \
                { c {return [string trim $c]}}]
    lassign $line size resident shared text lib data dt
    dict set statm size $size
    dict set statm resident $resident
    dict set statm shared $shared
    dict set statm text $text
    dict set statm lib $lib
    dict set statm data $data
    dict set statm dt $dt

    return $statm
}


# ::procfs::meminfo -- Memory informaton
#
#      Returns a dictionary with memory information for the entire system. The
#      entire list of keys is described in the /proc/meminfo section of the
#      manual. Memory sizes will automatically be converted to amount of bytes.
#
# Arguments:
#      None.
#
# Results:
#      Dictionary with keys matching meminfo information
#
# Side Effects:
#      None.
proc ::procfs::meminfo {} {
    set meminfo [Colon2Dict [file join ${vars::-proc} meminfo]]
    # Convert to # of bytes.
    dict for {k v} $meminfo {
        dict set meminfo $k [MemSizeConvert $v]
    }
    return $meminfo
}


proc ::procfs::cpuinfo { {convert true} } {
    # Mapper for easier (converted) keys. See lscpu implementation at:
    # https://github.com/karelzak/util-linux/blob/f0af42b51761428bdd821b31381fbfa1346f6782/sys-utils/lscpu.c#L379
    set mapper {
        vendor {vendor vendor_id "CPU implementer"}
        family {family "cpu family"}
        model {model "CPU part"}
        modelname {"model name"}
        stepping {stepping "CPU variant"}
        mhz {"cpu MHz"}
        dynamic_mhz {"cpu MHz dynamic"}
        static_mhz {"cpu MHz static"}
        flags {flags features Features type}
        bogomips {bogomips BogoMIPS "bogomips per cpu"}
        cpu {cpu}
        revision {revision "CPU revision"}
        mtid {"max thread id"}
        addrsz {"address sizes"}
    }
    set cpus [list]
    set cpu [dict create]
    foreach line [Lines [file join ${vars::-proc} cpuinfo] {} -trim] {
        if { $line eq "" } {
            if { [dict size $cpu] > 0 } {
                # Convert to somewhat unified across architectures and
                # implementations.
                if { $convert } {
                    set cvt [dict create]
                    dict for {k v} $cpu {
                        set converted 0
                        dict for {c allkeys} $mapper {
                            if { [lsearch -exact $allkeys $k] >= 0 } {
                                dict set cvt $c $v
                                set converted 1
                                break
                            }
                        }
                        # Keep the key if we didn't recognise it.
                        if {!$converted} {
                            dict set cvt $k $v
                        }
                    }
                    lappend cpus $cvt
                } else {
                    lappend cpus $cpu
                }
                # Start a new dictionary for next CPU information
                set cpu [dict create]
            }
        } else {
            set idx [string first ":" $line]
            if { $idx >= 0 } {
                set k [string trim [string range $line 0 [expr {$idx-1}]]]
                set v [string trim [string range $line [expr {$idx+1}] end]]
                dict set cpu $k $v
            }
        }
    }

    return $cpus
}


# ::procfs::status -- Process status
#
#      Return a dictionary with status for a given process. The keys
#      of the dictionary are as described in the /proc/[pid]/status section of
#      the manual. All known memory sizes will automatically be converted to
#      number of bytes.
#
# Arguments:
#      pid      Process identifier
#
# Results:
#      Return dictionary with keys matching status information.
#
# Side Effects:
#      None.
proc ::procfs::status { pid } {
    set status [Colon2Dict [file join ${vars::-proc} $pid status]]
    # Convert to # of bytes.
    foreach k [list VmPeak VmSize VmLck VmPin VmHWM VmRSS RssAnon RssFile RssShmem VmData VmStk VmExe VmLib VmPTE VmPWD VmSwap HugetlbPages] {
        if { [dict exists $status $k] } {
            dict set status $k [MemSizeConvert [dict get $status $k]]
        }
    }
    return $status
}

proc ::procfs::uptime { } {
    set uptime [GetFileContent [file join ${vars::-proc} uptime]]
    return [dict create uptime [lindex $uptime 0] idle [lindex $uptime 1]]
}


# ::procfs::Colon2Dict -- Convert colon led files
#
#      This procedure will return a dictionary representing the content of files
#      that are formatted with a colon separated key-value format.  All empty
#      lines will automatically be skipped. The keys and values are
#      automatically trimmed from leading and ending spaces in the resulting dictionary.
#
# Arguments:
#      fname    Path to file
#
# Results:
#      Dictionary representing file content.
#
# Side Effects:
#      None.
proc ::procfs::Colon2Dict { fname } {
    set d {}
    foreach line [Lines $fname {} -trim -skip] {
        set idx [string first ":" $line]
        if { $idx >= 0 } {
            set k [string trim [string range $line 0 [expr {$idx-1}]]]
            set v [string trim [string range $line [expr {$idx+1}] end]]
            dict set d $k $v
        }
    }
    return $d
}


# ::procfs::StatPID -- Implementation of PID stat
#
#      See main ::procfs::stat description
#
# Arguments:
#      pid      Process identifier
#      convert  Should we convert memory sizes
#
# Results:
#      A dictionary representing the /proc/[pid]/stat section of the manual
#
# Side Effects:
#      None.
proc ::procfs::StatPID { pid {convert false}} {
    set stat {}
    set line [GetFileContent [file join ${vars::-proc} $pid stat] {c {return [string trim $c]}}]
    append line [string repeat " 0" 20]; # This is to "create" values for things that appear in later kernels when running on an older kernel.
    scan $line "%d %s %c %d %d %d %d %d %u %lu %lu %lu %lu %lu %lu %ld %ld %ld %ld %ld %ld %lu %lu %ld %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %d %d %u %u %lu %lu %ld %lu %lu %lu %lu %lu %lu %lu %d" \
        id \
        comm \
        state \
        ppid \
        pgrp \
        session \
        tty_nr \
        tpgid \
        flags \
        minflt \
        cminflt \
        majflt \
        cmajflt \
        utime \
        stime \
        cutime \
        cstime \
        priority \
        nice \
        num_threads \
        itrealvalue \
        starttime \
        vsize \
        rss \
        rsslim \
        startcode \
        endcode \
        startstack \
        kstkesp \
        kstkeip \
        signal \
        blocked \
        sigignore \
        sigcatch \
        wchan \
        nswap \
        cnswap \
        exit_signal \
        processor \
        rt_priority \
        policy \
        delayacct_blkio_ticks \
        guest_time \
        cguest_time \
        start_data \
        end_data \
        start_brk \
        arg_start \
        arg_end \
        env_start \
        env_end \
        exit_code
    dict set stat pid $id
    dict set stat comm [string trim $comm "()"]
    dict set stat state $state
    dict set stat ppid $ppid
    dict set stat pgrp $pgrp
    dict set stat session $session
    dict set stat tty_nr $tty_nr
    dict set stat tpgid $tpgid
    dict set stat flags $flags
    dict set stat minflt $minflt
    dict set stat cminflt $cminflt
    dict set stat majflt $majflt
    dict set stat cmajflt $cmajflt
    dict set stat utime $utime
    dict set stat stime $stime
    dict set stat cutime $cutime
    dict set stat cstime $cstime
    dict set stat priority $priority
    dict set stat nice $nice
    dict set stat num_threads $num_threads
    dict set stat itrealvalue $itrealvalue
    dict set stat starttime $starttime
    dict set stat vsize $vsize
    dict set stat rss $rss
    dict set stat rsslim $rsslim
    dict set stat startcode $startcode
    dict set stat endcode $endcode
    dict set stat startstack $startstack
    dict set stat kstkesp $kstkesp
    dict set stat kstkeip $kstkeip
    dict set stat signal $signal
    dict set stat blocked $blocked
    dict set stat sigignore $sigignore
    dict set stat sigcatch $sigcatch
    dict set stat wchan $wchan
    dict set stat nswap $nswap
    dict set stat cnswap $cnswap
    dict set stat exit_signal $exit_signal
    dict set stat processor $processor
    dict set stat rt_priority $rt_priority
    dict set stat policy $policy
    dict set stat delayacct_blkio_ticks $delayacct_blkio_ticks
    dict set stat guest_time $guest_time
    dict set stat cguest_time $cguest_time
    dict set stat start_data $start_data
    dict set stat end_data $end_data
    dict set stat start_brk $start_brk
    dict set stat arg_start $arg_start
    dict set stat arg_end $arg_end
    dict set stat env_start $env_end
    dict set stat env_end $env_end
    dict set stat exit_code $exit_code
    if { $convert } {
        foreach tm [list utime stime cutime cstime starttime guest_time cguest_time] {
            dict set stat $tm [expr {[dict get $stat $tm] / [CLK_TCK]}]
        }
    }

    return $stat
}


# ::procfs::MemSizeConvert -- Convert memory size
#
#      Detect the presence of kB, MB, etc. at the end of a memory size
#      description and return the value in number of bytes.
#
# Arguments:
#      val      Incoming value, e.g. 23 kB
#
# Results:
#      An integer representing the incoming value in number of bytes
#
# Side Effects:
#      None.
proc ::procfs::MemSizeConvert { val } {
    lassign $val value unit
    switch -nocase -glob -- $unit {
        "kB" {
            set value [expr {1024*$value}]
        }
        "MB" {
            set value [expr {1024*1024*$value}]
        }
        "GB" {
            set value [expr {1024*1024*1024*$value}]
        }
        "TB" {
            set value [expr {1024*1024*1024*$value}]
        }
        "B" -
        "" {
            # Nothing on purpose
        }
        "*" {
            set value <NaN>
        }
    }
    return $value
}


# ::procfs::CLK_TCK -- System clock tick
#
#      Return the system-wide number of ticks per seconds. This is cached as it
#      requires calling getconf CLK_TCK.
#
# Arguments:
#      None.
#
# Results:
#      Number of ticks per second
#
# Side Effects:
#      Executes getconf to cache in the value.
proc ::procfs::CLK_TCK {} {
    # Cache (if necessary) system clock ticks. This is a constant.
    if { $vars::CLK_TCK < 0 } {
        # Integer rounding is a "good enough" approximation as we can't do this
        # in one single go.  It'll abstract away the time between the two
        # different "calls" within the procfs.
        set vars::CLK_TCK [expr {int(([dict get [stat self off] starttime] / [dict get [uptime] uptime])+0.5)}]
        # The following could also be used, but depends on the existence of
        # getconf, a binary that is not always available on linux, e.g.
        # RancherOS.
        #set vars::CLK_TCK [exec getconf CLK_TCK]
    }
    return $vars::CLK_TCK
}


# ::procfs::GetFileContent -- Get content of file
#
#      Get the content of a given file, possibly applying a (lambda)
#      transformation on the content. 
#
# Arguments:
#      fpath    Path to file to read
#      lambda   Lamba to apply on content (defaults to the content itself)
#      dft      Default value on lambda errors.
#
# Results:
#      (transformed) content of file, or default value.
#
# Side Effects:
#      None.
proc ::procfs::GetFileContent { fpath {lambda {c {return $c}}} { dft "" } } {
    set content $dft
    set fd [Open $fpath]
    try {
        set content [apply $lambda [read $fd]]
    } finally {
        Close $fd
    }
    return $content
}


# ::procfs::Lines -- Line-by-line file reading
#
#      Returns the (transformed) lines contained in a file. Each line will first
#      be passed to a lambda and the result of this lambda will then be
#      transformed further and possibly added to the list of lines depending on
#      the arguments.  The meaning of the dash-led arguments is as follows:
#      -trim to trim whitespaces from start and end of lambda-transformed line,
#      -trimleft to trim whitespace at left only, -trimright for right-side
#      trimming, -skip not to add lines that would be empty after
#      lambda-transformation and trimming.
#
# Arguments:
#      fpath    Path to file
#      lambda   Lambad to apply on each (original) line from file
#      args     dash-led arguments, see above.
#
# Results:
#      (cleaned) list of lambda-transformed lines from file content.
#
# Side Effects:
#      None.
proc ::procfs::Lines { fpath lambda args } {
    set lines [list]
    set fd [Open $fpath]
    try {
        while {![eof $fd]} {
            if { [llength $lambda] } {
                set line [apply $lambda [gets $fd]]
            } else {
                set line [gets $fd]
            }
            if { "-trim" in $args } { set line [string trim $line] }
            if { "-trimleft" in $args } { set line [string trimleft $line] }
            if { "-trimright" in $args } { set line [string trimright $line] }
            if { "-skip" ni $args || ( "-skip" in $args && $line ne "" ) } {
                lappend lines $line
            }
        }
    } finally {
        Close $fd
    }
    return $lines
}


proc ::procfs::Open { fpath args } {
    if { [llength ${vars::-hook}] } {
        return [{*}${vars::-hook} open $fpath {*}$args]
    } else {
        return [open $fpath {*}$args]
    }
}

proc ::procfs::Close { fd } {
    if { [llength ${vars::-hook}] } {
        return [{*}${vars::-hook} close $fd]
    } else {
        return [close $fd]
    }
}

proc ::procfs::Glob { directory pattern args } {
    if { [llength ${vars::-hook}] } {
        return [{*}${vars::-hook} glob $directory $pattern {*}$args]
    } else {
        return [glob -directory $directory {*}$args -- $pattern]
    }
}