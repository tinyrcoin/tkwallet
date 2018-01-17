catch {
package require Tk 8.4
option add *tearOff 0
}
package require http
package require msgcat
namespace import msgcat::mc
set wallet "default"
catch { wm geometry . 800x480 }
proc settitle {walname} { wm title . "TkWallet3x (RCoinX)" }
catch {settitle $wallet}
array set opts [list \
	--language-dir "%s" \
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
	puts ">> $query"
	puts "<< [http::data $k]"
	array set r [json2dict [http::data $k]]
	http::cleanup $k
	return $r(result)
}
array set opts $argv
 proc encsource {source_file encoding} {
     if {![catch {open $source_file r} fid]} {
       if {![catch {fconfigure $fid -encoding $encoding} msg]} {
         set script [read $fid]
         catch {close $fid}
       } else {
         # make sure channel gets closed
         catch {close $fid}
         return -code error "unknown encoding \"$encoding\""
       }
     } else {
         # return error message similar to source cmd
         return -code error "couldn't read file \"$source_file\": no such file or directory"
     }
     # not sure if this has to be catched as well to propagate the error code to the caller
     # to imitate the original source cmds behaviour.
     uplevel 1 $script
 }

if ![info exists opts(--no-translations)] {
	foreach k [glob -nocomplain -d [format $opts(--language-dir) $mepath] *.msg] {
		encsource $k utf-8
	}
}
if {$opts(--daemon-host) eq "localhost" && $opts(--lightweight)} {
	set opts(--daemon-host) 5.135.179.19
}
menu .menu
menu .menu.mine
.menu.mine add command -label [mc "Launch Miner"] -command {
	exec cmd /c start [format $opts(--miner-path) $mepath] --address $curaddr --log-level 4 --threads 1 --daemon-rpc-port $opts(--daemon-port) --daemon-host $opts(--daemon-host) &
}
.menu add cascade -label [mc "Mining"] -menu .menu.mine
menu .menu.help
.menu.help add command -label [mc "About"] -command {
	tk_messageBox -title "About TkWallet3x" -message "TkWallet3x (C) 2017, 2018 Ronsor.\nThis is FREE SOFTWARE licensed under the MIT LICENSE.\nThis TkWallet build is BETA. We can't guarantee it won't flush your coins down the toilet!"
}
.menu.help add command -label [mc "Anonymous Message"] -command {
	tk_messageBox -title [mc "Anonymous Message Info"] -message [mc "An anonymous message can be sent instead of coins.\nIt costs only the network transaction fee and the recipient will not know who the message is from.\nThe 'amount' field will be disregarded if you send an anonymous message.\n"]
}
.menu add cascade -label [mc "Help"] -menu .menu.help
. configure -menu .menu
wm withdraw .
proc passdlg a {
	toplevel .pwd
	if {!$a} {
		wm title .pwd [mc "Wallet Creation"]
		label .pwd.l -text [mc "You are creating a new wallet. Enter your desired password below."]
		grid x .pwd.l -padx 4 -pady 4 -sticky nw
		label .pwd.lp -text [mc "Password:"]
		entry .pwd.ep -width 40 -show * -textvariable pswd
		grid .pwd.lp .pwd.ep -sticky nw -padx 4 -pady 4
		label .pwd.lcp -text [mc "Confirm:"]
		entry .pwd.ecp -width 40 -show * -textvariable cpswd
		grid .pwd.lcp .pwd.ecp -sticky nw -padx 4 -pady 4
		bind .pwd <Destroy> exit
		button .pwd.ok -text [mc "Create Wallet"] -command {
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
		wm title .pwd [mc "Wallet Login"]
		label .pwd.l -text [mc "You are accessing a wallet. Enter your password below."]
		grid x .pwd.l -padx 4 -pady 4 -sticky nw
		label .pwd.lp -text [mc "Password:"]
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
proc daemontest {} {
	set r [http::geturl http://$::opts(--daemon-host):$::opts(--daemon-port)/getinfo]
	set d [http::data $r]
	http::cleanup $r
	return [json2dict $d]
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
	wm title .t [mc "Working"]
	label .t.l -text [mc "Waiting for RCoinX daemon..."]
	pack .t.l -ipadx 15 -ipady 15
	update
	after 1000; set n 0
	while {[catch {rpccall get_height}] || [catch {daemontest}]} {
		if {$n > 13} {
			tk_messageBox -icon error -title Failed -message [mc "Failed to start daemon.\nDid you supply an incorrect password?"]
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
	proc transfer {from to amount fee args} {
		array set r [rpccall transfer \
			mixin {-raw- 0} \
			fee [list -raw- [balconv $fee]] \
			unlock_time {-raw- 0} \
			destinations [list -raw- "\[ [dict2json [list amount [list -raw- [balconv $amount]] address $to] ] ]"] \
			{*}$args \
		]
		puts [array get r]
	}
	proc sendmsg {from to message fee} {
		#return [tk_messageBox -icon error -title Unsupported -message "TODO: fix anonymous messages"]
		set amount 1000001
		set id [format %04x [expr [clock clicks] % 65635]]
		set seq 0
		for {set seq 0} {$seq < 255 && ($seq * 27 < [string length $message])} {incr seq} {
		set msg [string range $message [expr $seq * 27] [expr $seq * 27 + 26]]
		array set r [rpccall transfer \
			mixin {-raw- 0} \
			fee [list -raw- [balconv $fee] ] \
			unlock_time {-raw- 0} \
			payment_id fefe$id[format %02x $seq][BIN2HEX $msg][string repeat 0 [expr 54 - [string length [BIN2HEX $msg]]]] \
			destinations [list -raw- "\[ [dict2json [list amount [list -raw- $amount] address $to] ] ]"] \
		]
		puts [array get r]
		}
	}
	proc getmsgs {} {
		array set ret {}
		array set r [rpccall get_transfers]
		foreach v $r(transfers) {
			array set q $v
			set pid [string tolower $q(paymentId)]
			if [string match fefe* $pid] {
				set id [scan [string range $pid 4 7] %x]
				set seq [scan [string range $pid 8 9] %x]
				puts "Seq: $seq [string range $pid 8 9]"
				set msg [string map {"\x00" ""} [HEX2BIN [string range $pid 10 end]]]
				dict set ret($id) $seq $msg
			}
		}
		set ret2 {}
		foreach {k v} [array get ret] {
			set sorted [lsort -integer -stride 2 -index 0 $v]
			lappend ret2 $k [join [dict values $sorted] ""]
		}
		return $ret2
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
	.menu add cascade -menu .menu.addr -label [mc Wallets]
}
proc chwallet x {
	set ::curaddr $x
}
proc convbal b {
	return [format "%u.%010u" [expr {$b / 10000000000}] [expr {abs($b % 10000000000)}]]
}
proc balconv b {
	return [lindex [split [expr {$b * 10000000000}] "."] 0]
}
set ::curheight 0
set ::msgsall {}
proc autobal {} {
	.info configure -text [mc "Balance: %s\nBlockchain Height: %s\n" [convbal [wallet::balance $::curaddr]] [wallet::height]]
	set ::statusbar [expr {[wallet::issyncing] ? "Syncing... [wallet::syncstat] blocks." : "Ready"}]
	set msgdict [wallet::getmsgs]
	set ::msgsall $msgdict
	set msgnames {}
	foreach {k v} $msgdict {
		set vv [split $v "\n"]
		if {[lindex $vv 0 0] eq "Subject:"} {
			lappend msgnames "$k [string map {"Subject: " ""} [lindex $vv 0]]"
		} else {
			lappend msgnames "$k (No Subject)"
		}
	}
	set ::msglists $msgnames
	after 3000 autobal
}
proc showcurmsg {} {
	catch {
	set id [lindex [split [.anonmsgs.m get [.anonmsgs.m curselection]]] 0]
	.anonmsgs.t delete 1.0 end
	.anonmsgs.t insert end "Message-ID: $id\n"
	.anonmsgs.t insert end "Message-Size: [string length [dict get $::msgsall $id]]\n"
	.anonmsgs.t insert end "\n"
	.anonmsgs.t insert end [dict get $::msgsall $id]
	}
}
listaddr
label .l -text [mc "Address:"]
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
labelframe .trans -text [mc "Transfer Coins or Message"] -padx 5 -pady 5
pack .trans -side top -fill x
label .trans.lto -text [mc "To:"] -anchor nw -justify left
entry .trans.to -textvariable trto -width 100
grid .trans.lto .trans.to -sticky nw
label .trans.lamt -text [mc "Amount:"] -anchor nw -justify left
entry .trans.amt -textvariable tramt -width 20
grid .trans.lamt .trans.amt -sticky nw
label .trans.lid -text [mc "Optional Payment ID:"] -anchor nw -justify left
entry .trans.id -textvariable trid -width 64
grid .trans.lid .trans.id -sticky nw
label .trans.lam -text [mc "Anonymous message:"]
#entry .trans.am -textvariable tram -width 100
set ::body ""
set ::subj "(No Subject)"

proc writeam {} {
	catch {unset ::exitmsg}
	toplevel .msg
	wm title .msg "Write Anonymous Message"
	bind .msg <Destroy> {set ::exitmsg -1}
	label .msg.lsubj -text [mc "Subject"]
	entry .msg.subj -textvariable subj -width 60 
	grid .msg.lsubj .msg.subj -ipadx 5 -ipady 5 -sticky nw
	text .msg.body
	.msg.body insert end $::body
	grid x .msg.body -sticky news -ipadx 5 -ipady 5
	button .msg.ok -text "Done" -command {set ::exitmsg 1}
	grid x .msg.ok -sticky se -ipadx 5 -ipady 5
	vwait ::exitmsg
	if {$::exitmsg == 1} {
		bind .msg <Destroy> {}
		set ::body [.msg.body get 1.0 end]
		destroy .msg
	}
	set ::tram "Subject: $::subj\n$::body"
}
button .trans.am -text [mc "Write Message (instead of sending coins)"] -command writeam
grid .trans.lam .trans.am -sticky nw
label .trans.lfee -text [mc "Current network fee:"]
set tfee [convbal 1000000]
entry .trans.fee -textvariable tfee
grid .trans.lfee .trans.fee -sticky nw
button .trans.send -text [mc "Send Coins or Message"] -command {apply {{} {
	if [wallet::issyncing] {
		tk_messageBox -icon error -title Error -message [mc "Wait for syncing to complete."]
		return
	}
	puts [mc "Fee: %s" $::tfee]
	set cmd {wallet::transfer $::curaddr $::trto $::tramt $::tfee}
	if {$::trid ne ""} {
		append cmd [list payment_id $::trid]
	}
	if {$::tram ne ""} {
		set cmd {wallet::sendmsg $::curaddr $::trto $::tram $::tfee}
	}
	if {[catch $cmd err]} {
		tk_messageBox -icon error -title "Error" -message "Not enough coins or the daemon is syncing."
		puts "Warning: Transaction failed: $err"
	} else {
		set ::trto ""
		set ::tramt 0
		set ::subj "(No Subject)"
		set ::tram ""
		set ::trid ""
	}
}}}
grid x .trans.send -sticky se
frame .anonmsgs
listbox .anonmsgs.m -yscrollcommand {.anonmsgs.ms set} -listvariable msglists -width 40
bind .anonmsgs.m <1> {showcurmsg}
scrollbar .anonmsgs.ms -orient vertical -command {.anonmsgs.m yview}
text .anonmsgs.t -yscrollcommand {.anonmsgs.s set}
scrollbar .anonmsgs.s -orient vertical -command {.anonmsgs.t yview}
label .anonmsgs.l -text "Received anonymous messages:"
pack .anonmsgs.m -side left -fill y
pack .anonmsgs.ms -side left -fill y
pack .anonmsgs.t -side left -expand yes -fill both
pack .anonmsgs.s -side left -fill y
pack .anonmsgs -expand yes -fill both

