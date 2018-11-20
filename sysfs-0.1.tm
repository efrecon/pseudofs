package require Tcl 8.6
package require pseudofs

namespace eval ::pseudofs::sys {
    namespace eval vars {
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
    namespace ensemble create -command ::sysfs
}

# ####### WARNING #########
#
# This imports a number of commands from the parent namespace, commands that
# have identical names to a number of VERY common Tcl commands for IO. The
# programmer should fully-qualify the original commands when writing code here!
#
# #######   EOW   #########

proc ::pseudofs::sys::lsblk { args } {
    pseudofs getopt args -keys keys { NAME RA RO RM ROTA RAND MODEL SERIAL SIZE STATE
                                      ALIGNMENT MIN-IO OPT-IO PHY-SEC LOG-SEC RQ-SIZE
                                      DISC-ALN DISC-GRAN DISC-MAX DISC-ZERO WSAME
                                      VENDOR REV ZONED SECTOR }
    pseudofs getopt args -allow allow "*"
    pseudofs getopt args -deny deny ""

    set devices [list]
    set devs [glob [::file join [configure -sys] block] * -types d -tails -nocomplain]
    set locator {
        RA queue/read_ahead_kb 0
        RO ro 0
        RM removable 0
        MODEL device/model ""
        SERIAL device/serial ""
        ROTA queue/rotational 1
        RAND queue/add_random 1
        STATE device/state ""
        ALIGNMENT alignment_offset 0
        MIN-IO queue/minimum_io_size -1
        OPT-IO queue/optimal_io_size -1
        PHY-SEC queue/physical_block_size -1
        LOG-SEC queue/logical_block_size -1
        RQ-SIZE queue/nr_requests -1
        DISC-ALN discard_alignment 0
        DISC-GRAN queue/discard_granularity -1
        DISC-MAX queue/discard_max_bytes -1
        DISC-ZERO queue/discard_zeroes_data 0
        WSAME queue/write_same_max_bytes 0
        VENDOR device/vendor ""
        ZONED queue/zoned ""
        REV device/rev ""
        SECTOR queue/hw_sector_size 0
    }
    foreach d $devs {
        if { [string match $allow $d] && ![string match $deny $d] } {
            set dev [dict create]
            set rootdir [::file join [configure -sys] block $d]
            foreach k $keys {
                switch -nocase -- $k {
                    "NAME" {
                        dict set dev $k $d
                    }
                    "RA" -
                    "RO" -
                    "RM" -
                    "MODEL" -
                    "SERIAL" -
                    "ROTA" -
                    "RAND" -
                    "STATE" -
                    "ALIGNMENT" -
                    "MIN-IO" -
                    "OPT-IO" -
                    "PHY-SEC" -
                    "LOG-SEC" -
                    "RQ-SIZE" -
                    "DISC-ALN" -
                    "DISC-GRAN" -
                    "DISC-MAX" -
                    "DISC-ZERO" -
                    "WSAME" -
                    "VENDOR" -
                    "ZONED" -
                    "REV" -
                    "SECTOR" {
                        foreach {key subdir dft} $locator {
                            if { [string equal -nocase $key $k] } {
                                dict set dev $key [file [::file join $rootdir $subdir] -trim -default $dft -nofail]
                            }
                        }
                    }
                    "SIZE" {
                        set sectors [file [::file join $rootdir size] -trim -default 0 -nofail]
                        set secsize [file [::file join $rootdir queue hw_sector_size] -trim -default 0 -nofail]
                        dict set dev SIZE [expr {$sectors*$secsize}]
                    }
                    default {
                        return -code error "$k is not a known/supported lsblk key!"
                    }
                }
            }
            lappend devices $dev
        }
    }

    return $devices
}