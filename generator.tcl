#!/bin/sh
# -*- tcl -*-
# The next line is executed by /bin/sh, but not tcl \
exec tclsh "$0" ${1+"$@"}

# Generate data for testing decathlon program

proc getRandomScore {er} {
    lassign $er a b
    set r [expr {rand()}]
    return [format "%.2f" [expr {$a*(1-$r) + $b*$r}]]
}

proc generateData {n_m n_c} {

    # Set names 100 girls and 100 boys
    set baseNames {Oliver Jack Charlie Harry Jacob George Noah Alfie Oscar James Freddie Leo Thomas William Archie Joshua Max Logan Henry Ethan Lucas Mason Theo Isaac Daniel Harrison Joseph Jake Alexander Finley Riley Samuel Edward Dylan Adam Teddy Benjamin Sebastian Lewis Arthur Toby Harley Jenson Tommy Jayden Matthew Frankie Alex Harvey Luke Ollie Jackson Blake David Bobby Ryan Zachary Tyler Elijah Luca Michael Rory Callum Jude Nathan Hugo Liam Stanley Louie Reuben Jaxon Elliot Elliott Ronnie Jamie Kai Connor Aaron Caleb Leon Louis Kian Dexter Finlay Reggie Carter Ben Seth Austin Roman Gabriel Cody Owen Sam Sonny Billy Aiden Ellis Cameron Albert Amelia Olivia Emily Isla Ava Poppy Sophie Jessica Lily Sophia Mia Ruby Grace Evie Isabella Ella Freya Chloe Scarlett Sienna Isabelle Lucy Daisy Millie Eva Charlotte Lilly Layla Alice Phoebe Molly Lola Ellie Rosie Erin Elsie Holly Lexi Sofia Esme Florence Imogen Bella Evelyn Maisie Willow Emma Lacey Summer Matilda Hannah Georgia Elizabeth Ivy Gracie Abigail Katie Amber Jasmine Rose Darcy Eliza Leah Annabelle Maya Megan Anna Brooke Faith Amelie Amy Violet Harriet Darcey Lottie Zara Harper Thea Eleanor Heidi Orla Mollie Tilly Martha Paige Emilia Darcie Hollie Isabel Bethany Robyn Skye Mila Maddison Victoria Zoe Lydia Madison Annie Julia Simon Karen Alana Norma Sanchi}

    # Create more if necessary
    set n_c_required [expr {$n_c-[llength $baseNames]}]
    if {$n_c_required > 0 } {
        # Append and prepend a-z
        set nNames {}
        foreach bn $baseNames {
            foreach x {a b c d e f g h i j k l m n o p q r s t u v w x y z} {
                lappend nNames "$bn-$x"
                incr n_c_required -1
                if {$n_c_required > 0 } {
                    lappend nNames "$x-$bn"
                    incr n_c_required -1
                }
                if {$n_c_required == 0 } {
                    break
                }
            }
            if {$n_c_required == 0 } {
                break
            }
        }
        set oNames [lsort [concat $baseNames $nNames]]
        
    } elseif {$n_c_required <  0} {
        set oNames [lreplace $baseNames [expr {$n_c}] end]
        
    }

    # Events dear boy
    set events {100m 110m 400m 1500m Discus Javelin Shot Long High Pole}
    set eventsRange {{10 18} {20 30} {65 85} {400 570} {4.1 16} {7.1 19} {1.6 8.5} {220.5 630} {75 99.5} {100 205}}
    
    # For each name and event combination create a random score
    set ofh [open "Decathlon.dat" w]
    for {set i 1} {$i <= $n_m} {incr i} {
        foreach n $oNames {
            foreach e $events er $eventsRange {
                puts $ofh "$n\t$e   \t[getRandomScore $er]"
            }
        }
        puts $ofh "#"
    }
    puts $ofh "##"
    close $ofh
}

#==============================================================================

if {$argc == 2} {
    lassign $argv n_m n_c
} else {
    puts -nonewline "Number of competitors > "; flush stdout
    set n_c [gets stdin]
    if {! [string is integer -strict $n_c]} {
        exit 1
    }
    puts -nonewline "Number of meetings > "; flush stdout
    set n_m [gets stdin]
    if {! [string is integer -strict $n_m]} {
        exit 1
    }
}
puts "Creating data for $n_m meetings (datasets) each with $n_c competitors"
generateData $n_m $n_c

