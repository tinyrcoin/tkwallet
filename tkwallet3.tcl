catch {
package require Tk 8.4
option add *tearOff 0
}
package require http
set wallet "default"
catch { wm geometry . 800x480 }
proc settitle {walname} { wm title . "TkWallet3x for RCoinX (${walname})" }
catch {settitle $wallet}
array set opts [list \
	--daemon-port 12991 \
	--daemon-host localhost \
	--walletd-path "%s/simplewallet[expr {$tcl_platform(os) eq "windows" ? ".exe" : ""}]" \
	--miner-path "%s/miner[expr {$tcl_platform(os) eq "windows" ? ".exe" : ""}]" \
	--daemon-path "%s/forknoted[expr {$tcl_platform(os) eq "windows" ? ".exe" : ""}]" \
	--config-file "[file join %s coin.conf]" \
	--lightweight "0" \
	--wallet-rpc-bind-port "34231" \
	--container-file "[file join $::env(HOME) rcoinx.v3]" \
]
set mepath [file dirname [info script]]
catch {
package require starkit
if { [string match "*.exe*" [info script]] } {
set mepath [file dirname [info nameofexecutable]]
}
}
if { [string match "*--help*" $argv] } {
	puts {Usage: tkwallet3x [--walletd-path path/to/simplewallet] [--config-file path/to/coin.conf] [--lightwallet 1|0] [--miner-path path/to/miner.exe]}
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
	set k [http::geturl "http://127.0.0.1:$::opts(--wallet-rpc-bind-port)/json_rpc" -headers {Accept application/json Content-Type application/json} -query $query]
	puts $query
	array set r [json2dict [http::data $k]]
	http::cleanup $k
	puts [array get r]
	return $r(result)
}
array set opts $argv
if {$opts(--daemon-host) eq "localhost" && $opts(--lightweight)} {
	set opts(--daemon-host) 5.135.179.19
}
menu .menu
menu .menu.mine
.menu.mine add command -label "Launch Miner" -command {
	exec cmd /c start [format $opts(--miner-path) $mepath] --address $curaddr --log-level 4 --threads 1 --daemon-rpc-port $opts(--daemon-port) --daemon-host $opts(--daemon-host) &
}
.menu add cascade -label "Mining" -menu .menu.mine
menu .menu.help
.menu.help add command -label "About" -command {
	tk_messageBox -title "About TkWallet3x" -message "TkWallet3x (C) 2017, 2018 Ronsor.\nThis is FREE SOFTWARE licensed under the MIT LICENSE.\nThis TkWallet build is BETA. We can't guarantee it won't flush your coins down the toilet!"
}
.menu.help add command -label "Anonymous Message" -command {
	tk_messageBox -title "Anonymous Message Info" -message "An anonymous message can be sent instead of coins.\nIt costs only the network transaction fee and the recipient will not know who the message is from.\nThe 'amount' field will be disregarded if you send an anonymous message.\n"

}
.menu add cascade -label "Help" -menu .menu.help
. configure -menu .menu
wm withdraw .
proc passdlg a {
	toplevel .pwd
	if {!$a} {
		wm title .pwd "Wallet Creation"
		label .pwd.l -text "You are creating a new wallet. Enter your desired password below."
		grid x .pwd.l -padx 4 -pady 4 -sticky nw
		label .pwd.lp -text "Password:"
		entry .pwd.ep -width 40 -show * -textvariable pswd
		grid .pwd.lp .pwd.ep -sticky nw -padx 4 -pady 4
		label .pwd.lcp -text "Confirm:"
		entry .pwd.ecp -width 40 -show * -textvariable cpswd
		grid .pwd.lcp .pwd.ecp -sticky nw -padx 4 -pady 4
		bind .pwd <Destroy> exit
		button .pwd.ok -text "Create Wallet" -command {
			if {$pswd ne $cpswd} {
				tk_messageBox -icon -error -title "Error" -message "Passwords do not match"
			} else {
				set pwdone 1
			}
		}
		grid x .pwd.ok -sticky se -padx 4 -pady 4
		vwait ::pwdone
		bind .pwd <Destroy> {}
		destroy .pwd
		return $::pswd
	} else {
		wm title .pwd "Wallet Login"
		label .pwd.l -text "You are accessing a wallet. Enter your password below."
		grid x .pwd.l -padx 4 -pady 4 -sticky nw
		label .pwd.lp -text "Password:"
		entry .pwd.ep -width 40 -show * -textvariable pswd
		grid .pwd.lp .pwd.ep -sticky nw -padx 4 -pady 4
		bind .pwd <Destroy> exit
		button .pwd.ok -text "Login" -command {
			set pwdone 1
		}
		grid x .pwd.ok -sticky se -padx 4 -pady 4
		vwait ::pwdone
		bind .pwd <Destroy> {}
		destroy .pwd
		return $::pswd
	}
}
if { [catch {rpccall get_height}] } {
	set passwd [passdlg [file exists $opts(--container-file).wallet]]
	if !$opts(--lightweight) { exec [format $opts(--daemon-path) $mepath] --config-file [format $opts(--config-file) $mepath] &; }
	if ![file exists $opts(--container-file).wallet] {
	set b [list [format $opts(--walletd-path) $mepath] --daemon-port $opts(--daemon-port) --daemon-host $opts(--daemon-host) --set_log 5 [expr {[file exists $opts(--container-file).wallet] ? "--wallet-file" : "--generate-new-wallet"}] $opts(--container-file) --pass $passwd --config-file [format $opts(--config-file) $mepath]]
	set f [open "|$b" r+]
	after 1000
	close $f
	}
	set a [list [format $opts(--walletd-path) $mepath] --daemon-port $opts(--daemon-port) --daemon-host $opts(--daemon-host) --set_log 5 [expr {[file exists $opts(--container-file).wallet] ? "--wallet-file" : "--generate-new-wallet"}] $opts(--container-file) --pass $passwd --config-file [format $opts(--config-file) $mepath] --wallet-rpc-bind-port=$opts(--wallet-rpc-bind-port) &]
	exec {*}$a
	puts $a
	wm withdraw .
	toplevel .t
	wm title .t "Working"
	label .t.l -text "Waiting for RCoinX daemon..."
	pack .t.l -ipadx 15 -ipady 15
	update
	after 1000; set n 0
	while {[catch {rpccall get_height}]} {
		if {$n > 10} {
			tk_messageBox -icon error -title Failed -message "Failed to start daemon.\nDid you supply an incorrect password?"
			exit
		}
		after 1000
		incr n
	}
	destroy .t
	update
	wm deiconify .
}
wm deiconify .

namespace eval wallet {
	proc addresslist {} {
		set f [open $::opts(--container-file).address]
		gets $f ret
		close $f
		return $ret
	}
	proc balance {addr} {
		array set r [rpccall getbalance]
		return [expr {$r(available_balance) + $r(locked_amount)}]
	}
	proc peers {} {
		array set r [::wallet::daemoninfo]
		return [expr {$r(outgoing_connections_count)+$r(incoming_connections_count)}]
	}
	proc difficulty {} {
		array set r [::wallet::daemoninfo]
		return $r(difficulty)
	}
	proc height {} {
		array set r [rpccall get_height]
		return $r(height)
	}
	proc daemoninfo {} {
		set r [http::geturl http://$::opts(--daemon-host):$::opts(--daemon-port)/getinfo]
		set d [http::data $r]
		http::cleanup $r
		return [json2dict $d]
	}
	# "alt_blocks_count":0,"difficulty":4666,
	# "grey_peerlist_size":30,"height":723,"incoming_connections_count":6,
	# "last_known_block_index":14847,"outgoing_connections_count":3,"status":"OK","tx_count":4,"tx_pool_size":0,"white_peerlist_size":3
	proc issyncing {} {
		array set r [::wallet::daemoninfo]
		return [expr {$r(height) < $r(last_known_block_index)}]
	}
	proc syncstat {} {
		array set r [::wallet::daemoninfo]
		return $r(height)/$r(last_known_block_index)
	}
	proc transfer {from to amount args} {
		array set r [rpccall transfer \
			mixin {-raw- 0} \
			fee {-raw- 1000000} \
			destinations [list -raw- "\[ [dict2json [list amount [list -raw- [balconv $amount]] address $to] ] ]"] \
			{*}$args \
		]
		puts [array get r]
	}
	proc sendmsg {from to message} {
		return [tk_messageBox -icon error -title Unsupported -message "TODO: fix anonymous messages"]
		set amount 1
		array set r [rpccall sendTransaction \
			anonymity {-raw- 0} \
			fee {-raw- 1000000} \
			extra [BIN2HEX $message] \
			transfers [list -raw- "\[ [dict2json [list amount [list -raw- [balconv $amount]] address $to] ] ]"] \
		]
		puts [array get r]
	}
	proc getmsgs {max {start 0}} {
		set ret {}
		array set r [rpccall getTransactions blockCount [list -raw- $max] firstBlockIndex [list -raw- $start]]
		foreach v $r(items) {
			array set q $v
			foreach t $q(transactions) {
				array set tt $t
				if { [string length $tt(extra)] > 66 } {
					lappend ret "[clock format $tt(timestamp)]: [HEX2BIN [string range $tt(extra) 66 end]]"
				}
			}
		}
		return $ret
	}
}
proc BIN2HEX { text }   { binary scan $text H* result; return $result }
proc HEX2BIN { hex }    { return [binary format H* $hex] }
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
	return [format "%u.%010u" [expr {$b / 10000000000}] [expr {abs($b % 10000000000)}]]
}
proc balconv b {
	return [expr {$b * 10000000000}]
}
set ::curheight 0
proc autobal {} {
	.info configure -text "Balance: [convbal [wallet::balance $::curaddr]]\nBlockchain Height: [wallet::height]\nPeers: [wallet::peers]\n"
	set ::statusbar [expr {[wallet::issyncing] ? "Syncing... [wallet::syncstat] blocks." : "Ready"}]
	if 0 {
	set msgs [wallet::getmsgs 1000000 $::curheight]
	if {$msgs ne ""} {
	.anonmsgs.t insert end "[join $msgs "\n"]\n"
	}
	array set r [rpccall getStatus]
	set ::curheight $r(blockCount)
	}
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
labelframe .trans -text "Transfer Coins or Message" -padx 5 -pady 5
pack .trans -side top -fill x
label .trans.lto -text "To:" -anchor nw -justify left
entry .trans.to -textvariable trto -width 100
grid .trans.lto .trans.to -sticky nw
label .trans.lamt -text "Amount:" -anchor nw -justify left
entry .trans.amt -textvariable tramt -width 20
grid .trans.lamt .trans.amt -sticky nw
label .trans.lid -text "Optional Payment ID:" -anchor nw -justify left
entry .trans.id -textvariable trid -width 64
grid .trans.lid .trans.id -sticky nw
label .trans.lam -text "Anonymous message:"
entry .trans.am -textvariable tram -width 100
grid .trans.lam .trans.am -sticky nw
label .trans.lfee -text "Current network fee:"
label .trans.fee -text [convbal 1000000]
grid .trans.lfee .trans.fee -sticky nw
button .trans.send -text "Send Coins" -command {apply {{} {
	if [wallet::issyncing] {
		tk_messageBox -icon error -title Error -message "Wait for syncing to complete."
		return
	}
	set cmd {wallet::transfer $::curaddr $::trto $::tramt}
	if {$::trid ne ""} {
		append cmd [list payment_id $::trid]
	}
	if {$::tram ne ""} {
		set cmd {wallet::sendmsg $::curaddr $::trto $::tram}
	}
	if [catch $cmd] {
		tk_messageBox -icon error -title "Error" -message "Not enough coins or the daemon is syncing."
	} else {
		set ::trto ""
		set ::tramt 0
		set ::tram ""
		set ::trid ""
	}
}}}
grid x .trans.send -sticky se
frame .anonmsgs
text .anonmsgs.t -yscrollcommand {.anonmsgs.s set}
scrollbar .anonmsgs.s -orient vertical -command {.anonmsgs.t yview}
label .anonmsgs.l -text "Received anonymous messages:"
pack .anonmsgs.l
pack .anonmsgs.t -side left -expand yes -fill both
pack .anonmsgs.s -side right -fill y
pack .anonmsgs -expand yes -fill both
