catch {
package require Tk 8.4
}
package require http
set wallet "default"
catch { wm geometry . 720x480 }
proc settitle {walname} { wm title . "TkWallet2X for RCoinX (${walname})" }
settitle $wallet
array set opts [list \
	--daemon-port 12991 \
	--daemon-host localhost \
	--walletd-path "%s/walletd[expr {$tcl_platform(os) eq "windows" ? ".exe" : ""}]" \
	--miner-path "%s/miner[expr {$tcl_platform(os) eq "windows" ? ".exe" : ""}]" \
	--config-file "%s/coin.conf" \
	--lightwallet "0" \
	--wallet-rpc-bind-port "34230"
]
set mepath [file dirname [info script]]
if { [string match "*--help*" $argv] } {
	puts {Usage: tkwallet2x [--walletd-path path/to/walletd] [--config-file path/to/coin.conf] [--lightwallet 1|0] [--miner-path path/to/miner.exe]}
	puts {}
	puts {Defaults:}
	foreach {k v} [array get opts] {
		puts "    [format "%-15s %s" $k [format $v $mepath]]"
	}
	exit 1
}
proc dict2json {dictionary} {
    foreach {key value} $dictionary {
        if {[string match {\[*\]} $value]} {
            lappend Result "\"$key\":$value"
	} elseif {$value eq {}} {
	    lappend Result "\"$key\":{}"
	} elseif {[lindex $value 0] eq "-raw-"} {
	    lappend Result "\"$key\":[lindex $value 1]"
        } elseif {([llength $value] % 2) == 0} {
            lappend Result "\"$key\":[dict2json $value]"
        } else {
            lappend Result "\"$key\":\"$value\""
        }
    }
    return "\{[join $Result ",\n"]\}"
}
proc json2dict {json {indexVar {}}} {
    # Link to the caller's index variable.
    if {$indexVar ne {}} {
        upvar 1 $indexVar index
    }

    # By default, start decoding at the start of the input.
    if {![info exists index]} {
        set index 0
    }

    # Skip leading whitespace.  Return empty at end of input.
    if {![regexp -indices -start $index {[^\t\n\r ]} $json range]} {
        return
    }
    set index [lindex $range 0]

    # The first character determines the JSON element type.
    switch [string index $json $index] {
    \" {
        # JSON strings start with double quote.
        set type string

        # The value is the text between matching double quotes.
        if {![regexp -indices -start $index {\A\"((?:[^"]|\\.)*)\"}\
                $json range sub]} {
            return -code error "invalid JSON string at index $index:\
                    must end with close quote"
        }
        set value [string range $json {*}$sub]

        # Process all backslash substitutions in the value.
        set start 0
        while {[regexp -indices -start $start {\\u[[:xdigit:]]{4}|\\[^u]}\
                $value sub]} {
            set char [string index $value [expr {[lindex $sub 0] + 1}]]
            switch $char {
                u {set char [subst [string range $value {*}$sub]]}
                b {set char \b} f {set char \f} n {set char \n}
                r {set char \r} t {set char \t}
            }
            set value [string replace $value {*}$sub $char]
            set start [expr {[lindex $sub 0] + 1}]
        }
    } \{ - \[ {
        # JSON objects/arrays start with open brace/bracket.
        if {[string index $json $index] eq "\{"} {
            set type object
            set endRe {\A[\t\n\r ]*\}}
            set charName brace
        } else {
            set type array
            set endRe {\A[\t\n\r ]*\]}
            set charName bracket
        }
        set value {}
        incr index

        # Loop until close brace/bracket is encountered.
        while {![regexp -indices -start $index $endRe $json range]} {
            # Each element other than the first is preceded by comma.
            if {[llength $value]} {
                if {![regexp -indices -start $index\
                        {\A[\t\n\r ]*,} $json range]} {
                    return -code error "invalid JSON $type at index $index:\
                            element not followed by comma or close $charName"
                }
                set index [expr {[lindex $range 1] + 1}]
            }

            # For objects, get key and confirm it is followed by colon.
            if {$type eq "object"} {
                set key [json2dict $json index]
		set key [list string $key]
                if {![llength $key]} {
                    return -code error "invalid JSON object at index $index:\
                            must end with close brace"
                } elseif {[lindex $key 0] ne "string"} {
                    return -code error "invalid JSON object at index $index:\
                            key type is \"[lindex $key 0]\", must be string"
                } elseif {![regexp -indices -start $index {\A[\t\n\r ]*:}\
                        $json range]} {
                    return -code error "invalid JSON object at index $index:\
                            key not followed by colon"
                }
                set index [expr {[lindex $range 1] + 1}]
                lappend value [lindex $key 1]
            }

            # Get element value.
            lappend value [json2dict $json index]
        }
    } t - f - n {
        # JSON literals are true, false, or null.
        set type literal
        if {![regexp -indices -start $index {(?:true|false|null)\M}\
                $json range]} {
            return -code error "invalid JSON literal at index $index"
        }
        set value [string range $json {*}$range]
    } - - + - 0 - 1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - . {
        # JSON numbers are integers or real numbers.
        set type number
        if {![regexp -indices -start $index --\
                {-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][-+]?\d+)?\M} $json range]} {
            return -code error "invalid JSON number at index $index"
        }
        set value [string range $json {*}$range]
    } default {
        # JSON allows only the above-listed types.
        return -code error "invalid JSON data at index $index"
    }}

    # Continue decoding after the last character matched above.
    set index [expr {[lindex $range 1] + 1}]

    # When performing a full decode, ensure only whitespace appears at end.
    if {$indexVar eq {} && [regexp -start $index {[^\t\n\r\ ]} $json]} {
        return -code error "junk at end of JSON"
    }

    # Return the type and value.
    return $value
}
proc rpccall {method args} {
	set query [dict2json [list \
		jsonrpc 2.0 \
		method $method \
		id none \
		params $args \
	]]
#	puts $query
	set k [http::geturl "http://127.0.0.1:$::opts(--wallet-rpc-bind-port)/json_rpc" -headers {Accept application/json Content-Type application/json} -query $query]
	array set r [json2dict [http::data $k]]
	http::cleanup $k
	if {$method eq "sendTransaction"} {
	puts [array get r]
	}
	return $r(result)
}
array set opts $argv
menu .menu
menu .menu.mine
.menu.mine add command -label "Launch Miner" -command {
	exec cmd /c start [format $opts(--miner-path) $mepath] --address $curaddr --log-level 4 --threads 1 --daemon-rpc-port $opts(--daemon-port) --daemon-host $opts(--daemon-host)
}
.menu add cascade -label "Mining" -menu .menu.mine
menu .menu.help
.menu.help add command -label "About" -command {
	tk_messageBox -title "About TkWallet2X" -message "TkWallet2X (C) 2017, 2018 Ronsor.\nThis is FREE SOFTWARE licensed under the MIT LICENSE.\n"
}
. configure -menu .menu
catch { exec [format $opts(--walletd-path) $mepath] -g --bind-port $opts(--wallet-rpc-bind-port) --config $opts(--config-file) >@stdout 2>@stderr }
if { [catch {rpccall getStatus}] } {
	exec [format $opts(--walletd-path) $mepath] --bind-port $opts(--wallet-rpc-bind-port) --config $opts(--config-file) --local 2>@stderr >@stdout &
	after 2000
	#while {[catch {rpccall getStatus}]} {after 1000}
}

namespace eval wallet {
	proc addresslist {} {
		array set r [rpccall getAddresses]
		return $r(addresses)
	}
	proc balance {addr} {
		array set r [rpccall getBalance address $addr]
		return [expr {$r(availableBalance) + $r(lockedAmount)}]
	}
	proc peers {} {
		array set r [rpccall getStatus]
		return $r(peerCount)
	}
	proc height {} {
		array set r [rpccall getStatus]
		return $r(blockCount)
	}
	proc issyncing {} {
		array set r [rpccall getStatus]
		return [expr {$r(blockCount) < $r(knownBlockCount)}]
	}
	proc syncstat {} {
		array set r [rpccall getStatus]
		return "$r(blockCount)/$r(knownBlockCount)"
	}
	proc transfer {from to amount} {
			#addresses [list -raw- "\[\"$from\"\]"]
		array set r [rpccall sendTransaction \
			anonymity {-raw- 0} \
			fee {-raw- 1000000} \
			transfers [list -raw- "\[ [dict2json [list amount [list -raw- [balconv $amount]] address $to] ] ]"] \
		]
		puts [array get r]
	}
}
proc listaddr {} {
	destroy .menu.addr
	menu .menu.addr
	foreach a [wallet::addresslist] {
		.menu.addr add command -label $a -command [list chwallet $a]
	}
	chwallet $a
	.menu add cascade -menu .menu.addr -label Wallets
}
proc chwallet x {
	set ::curaddr $x
}
proc convbal b {
	return [format "%d.%010d" [expr {$b / 10000000000}] [expr {$b % 10000000000}]]
}
proc balconv b {
	return [expr {$b * 10000000000}]
}
proc autobal {} {
	.info configure -text "Balance: [convbal [wallet::balance $::curaddr]]\nBlockchain Height: [wallet::height]\nPeers: [wallet::peers]\n"
	set ::statusbar [expr {[wallet::issyncing] ? "Syncing... [wallet::syncstat] blocks." : "Ready"}]

	after 1000 autobal
}
listaddr
label .l -text "Address:"
entry .e -textvariable curaddr
pack .l -fill x
pack .e -side top -fill x
bind .e <1> {
	clipboard append $::curaddr
}
after 1000 autobal
label .status -textvariable statusbar -anchor sw -justify left
pack .status -side bottom -fill x
label .info -text "" -anchor nw -justify left
pack .info -side top -fill x
labelframe .trans -text "Transfer Coins" -padx 5 -pady 5
pack .trans -side top -fill x
label .trans.lto -text "To:" -anchor nw -justify left
pack .trans.lto -fill x
entry .trans.to -textvariable trto
pack .trans.to -fill x
label .trans.lamt -text "Amount:" -anchor nw -justify left
pack .trans.lamt -fill x
entry .trans.amt -textvariable tramt -width 20
pack .trans.amt -anchor nw
button .trans.send -text "Send Coins" -command {apply {{} {
	if [wallet::issyncing] {
		tk_messageBox -icon error -title Error -message "Wait for syncing to complete."
		return
	}
	if [catch {wallet::transfer $::curaddr $::trto $::tramt}] {
		tk_messageBox -icon error -title "Error" -message "Not enough coins or bad address."
	} else {
		set ::trto ""
		set ::tramt 0
	}
}}}
pack .trans.send -anchor ne
