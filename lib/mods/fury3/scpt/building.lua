add_building_action
{
["index"] = 99,
["action"] = function()
remove_all_curse();
msg_print("You feel something watching over you.")
player.au = player.au - 10000
player.redraw = bor(player.redraw, PR_GOLD) 
end,
} 