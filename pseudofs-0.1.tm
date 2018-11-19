package require Tcl 8.6

namespace eval ::pseudofs {
    namespace eval vars {
        variable -proc   "/proc"
        variable -sys    "/sys"
        variable -hook   {}
    }
    namespace export {[a-z]*}
    namespace ensemble create
}

# ####### WARNING #########
#
# This implementation defines a number of commands/procs named identically to a
# number of VERY common Tcl commands for IO. The programmer is invited to
# fully-qualify the true Tcl commands in the root namespace whenever needing to
# call them!
#
# #######   EOW   #########


proc ::pseudofs::configure { args } {
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


# ::pseudofs::GetFileContent -- Get content of file
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
proc ::pseudofs::file { fpath args } {
    # Initialise
    getopt args -default content ""
    getopt args -lambda lambda {c {return $c}}

    # Open file, failing early or not
    if { [getopt args -nofail] } {
        if { [catch {open $fpath} fd] } {
            return $content
        }
    } else {
        set fd [open $fpath]
    }

    # Read content, apply lambda
    try {
        set content [read $fd]
        if { [getopt args -trim] } { set content [string trim $content] }
        if { [getopt args -trimleft] } { set content [string trimleft $content] }
        if { [getopt args -trimright] } { set content [string trimright $content] }
        set content [apply $lambda $content]
    } finally {
        close $fd
    }

    return $content
}


# ::pseudofs::Lines -- Line-by-line file reading
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
proc ::pseudofs::lines { fpath lambda args } {
    set lines [list]
    set fd [open $fpath]
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
        close $fd
    }
    return $lines
}


proc ::pseudofs::open { fpath args } {
    if { [llength ${vars::-hook}] } {
        return [{*}${vars::-hook} open $fpath {*}$args]
    } else {
        return [::open $fpath {*}$args]
    }
}

proc ::pseudofs::close { fd } {
    if { [llength ${vars::-hook}] } {
        return [{*}${vars::-hook} close $fd]
    } else {
        return [::close $fd]
    }
}

proc ::pseudofs::glob { directory pattern args } {
    if { [llength ${vars::-hook}] } {
        return [{*}${vars::-hook} glob $directory $pattern {*}$args]
    } else {
        return [::glob -directory $directory {*}$args -- $pattern]
    }
}

# ::pseudofs::Colon2Dict -- Convert colon led files
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
proc ::pseudofs::colon2dict { fname } {
    set d {}
    foreach line [lines $fname {} -trim -skip] {
        set idx [string first ":" $line]
        if { $idx >= 0 } {
            set k [string trim [string range $line 0 [expr {$idx-1}]]]
            set v [string trim [string range $line [expr {$idx+1}] end]]
            dict set d $k $v
        }
    }
    return $d
}

proc ::pseudofs::getopt {_argv name {_var ""} {default ""}} {
    upvar 1 $_argv argv $_var var
    set pos [lsearch -regexp $argv ^$name]
    if {$pos>=0} {
        set to $pos
        if {$_var ne ""} {
            set var [lindex $argv [incr to]]
        }
        set argv [lreplace $argv $pos $to]
        return 1
    } else {
        if {[llength [info level 0]] == 5} {set var $default}
        return 0
    }
}