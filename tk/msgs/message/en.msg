set loc en

# Window title
::msgcat::mcset $loc "Message History"

# Menu
::msgcat::mcset $loc Message
::msgcat::mcset $loc "Dump Messages"
::msgcat::mcset $loc "Combine Messages"
::msgcat::mcset $loc "Max Messages..."

# Search dialog
::msgcat::mcset $loc find-prompt Text

# Dialog
::msgcat::mcset $loc max-title "Number Of Displayed Messages"
::msgcat::mcset $loc max-prompt "Max (1-2048)"
::msgcat::mcset $loc max-message \
"Enter the number of messages to display. All the messages are \
kept, but if you choose a big number it takes longer to display."

# Messages Window: title
::msgcat::mcset $loc Messages

