package require Tcl 8.6
package require pseudofs

namespace eval ::pseudofs::proc {
    namespace eval vars {
        variable CLK_TCK -1
    }
    namespace export {[a-z]*}
    namespace import \
        [namespace parent]::configure \
        [namespace parent]::lines \
        [namespace parent]::file \
        [namespace parent]::open \
        [namespace parent]::close \
        [namespace parent]::glob \
        [namespace parent]::colon2dict
    namespace ensemble create -command ::procfs
}

# ####### WARNING #########
#
# This imports a number of commands from the parent namespace, commands that
# have identical names to a number of VERY common Tcl commands for IO. The
# programmer should fully-qualify the original commands when writing code here!
#
# #######   EOW   #########



# ::pseudofs::proc::pids -- List of processes
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
proc ::pseudofs::proc::pids {} {
    set pids [list]
    foreach dir [glob [configure -proc] * -types d -tails -nocomplain] {
        if { [string is integer -strict $dir] } {
            lappend pids $dir
        }
    }
    return $pids
}


# ::pseudofs::proc::cmdline -- Process command-line
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
proc ::pseudofs::proc::cmdline { pid } {
    return [file [::file join [configure -proc] $pid cmdline] \
                -lambda {cmdline {return [split [string trim $cmdline "\0"] "\0"]}}]
}


# ::pseudofs::proc::comm -- Process command
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
proc ::pseudofs::proc::comm { pid } {
    return [file [::file join [configure -proc] $pid comm] -trim]
}


# ::pseudofs::proc::io -- Disk I/O
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
proc ::pseudofs::proc::io { pid } {
    return [colon2dict [::file join [configure -proc] $pid io]]
}


# ::pseudofs::proc::interfaces -- List network interfaces
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
proc ::pseudofs::proc::interfaces { { pid -1 } }  {
    if { $pid >= 0 } {
        set fpath [::file join [configure -proc] $pid net dev]
    } else {
        set fpath [::file join [configure -proc] net dev]
    }
    return [lines $fpath \
                {line {
                    set idx [string first ":" $line]
                    if { $idx >= 0 } {
                        return [string range $line 0 [expr {$idx-1}]]
                    }
                }} -trim -skip]
}


# ::pseudofs::proc::netstats -- Process/System network stats
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
proc ::pseudofs::proc::netstats { { pid -1 } { direction "R" } { interface "*"} } {
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
        set fpath [::file join [configure -proc] $pid net dev]
    } else {
        set fpath [::file join [configure -proc] net dev]    
    }
    foreach line [lines $fpath {} -trim -skip] {
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


# ::pseudofs::proc::stat -- Process/system stat
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
proc ::pseudofs::proc::stat { { pid -1 } { convert false } } {
    if { $pid >= 0 } {
        return [StatPID $pid $convert]
    }
    set stat {}
    foreach line [lines [::file join [configure -proc] stat] {} -trim -skip] {
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


# ::pseudofs::proc::statm -- Memory statistics
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
proc ::pseudofs::proc::statm { pid } {
    set line [file [::file join [configure -proc] $pid statm] -trim]
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


# ::pseudofs::proc::meminfo -- Memory informaton
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
proc ::pseudofs::proc::meminfo {} {
    set meminfo [colon2dict [::file join [configure -proc] meminfo]]
    # Convert to # of bytes.
    dict for {k v} $meminfo {
        dict set meminfo $k [MemSizeConvert $v]
    }
    return $meminfo
}


proc ::pseudofs::proc::cpuinfo { {convert true} } {
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
    foreach line [lines [::file join [configure -proc] cpuinfo] {} -trim] {
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


# ::pseudofs::proc::status -- Process status
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
proc ::pseudofs::proc::status { pid } {
    set status [colon2dict [::file join [configure -proc] $pid status]]
    # Convert to # of bytes.
    foreach k [list VmPeak VmSize VmLck VmPin VmHWM VmRSS RssAnon RssFile RssShmem VmData VmStk VmExe VmLib VmPTE VmPWD VmSwap HugetlbPages] {
        if { [dict exists $status $k] } {
            dict set status $k [MemSizeConvert [dict get $status $k]]
        }
    }
    return $status
}

proc ::pseudofs::proc::uptime { } {
    set uptime [file [::file join [configure -proc] uptime] -trim]
    return [dict create uptime [lindex $uptime 0] idle [lindex $uptime 1]]
}





# ::pseudofs::proc::StatPID -- Implementation of PID stat
#
#      See main ::pseudofs::proc::stat description
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
proc ::pseudofs::proc::StatPID { pid {convert false}} {
    set stat {}
    set line [file [::file join [configure -proc] $pid stat] -trim]
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


# ::pseudofs::proc::MemSizeConvert -- Convert memory size
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
proc ::pseudofs::proc::MemSizeConvert { val } {
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


# ::pseudofs::proc::CLK_TCK -- System clock tick
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
proc ::pseudofs::proc::CLK_TCK {} {
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


