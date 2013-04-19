-- These constants aren't exported to Lua, so we define them here.
RUNE_SELF = 1
RUNE_ARROW = 2
RUNE_RAY = 4
RUNE_SPHERE = 8
RUNE_POWER_SURGE = 16
RUNE_ARMAGEDDON = 32
RUNE_CLOUD = 64
RUNE_CURTAIN = 128
RUNE_MOD_MAX = 8
RUNE_STONE = 255

-- Storage for our memorized runespells.
runespell_memory = {
  ["nsp"] = 0,
  ["nsav"] = 0
}
-- Dodgy Hack(TM) [see below]:  Reserve one key/number savefile slot to
-- store the number of memorized runespells, no matter what happens.
register_savefile(1)


-- New damage type GF_SAFEGRAVITY, to properly mimic the effect of a
-- Gravity/Self spell (see runespell_cast() below for full details).
GF_SAFEGRAVITY = add_spell_type
{
  ["color"] = { TERM_UMBER, 0 },
  ["angry"] = function() return TRUE, TRUE end,
  ["monster"] = function(who, dam, rad, y, x, monst)
    project(0, 0, y, x, dam, GF_GRAVITY, bor(PROJECT_JUMP, PROJECT_KILL, PROJECT_HIDE))
  end,
  ["player"] = function(who, dam, rad, y, x)
    msg_print("Gravity warps around you!")
    teleport_player(dam)
  end
}

-- Display the main meny of Runecraft powers.
function runecraft_menu()
  local ret = 0

  screen_save()
  while not nil do
    prt("  a) Cast from runes ",                1, 0)
    prt("  b) Cast memorized runespell ",       2, 0)
    prt("  c) Memorize runespell ",             3, 0)
    prt("  d) Forget memorized runespell ",     4, 0)
    prt("  e) List memorized runespells ",      5, 0)
    prt("  f) Cast from Runestone ",            6, 0)
    prt("  g) Examine Runestone ",              7, 0)
    prt("  h) Carve runespell onto Runestone ", 8, 0)
    prt("Select a Runecraft skill: ", 0, 0)

    local key = inkey()
    if key == ESCAPE then
      break
    elseif key >= strbyte("a") and key <= strbyte("h") then
      ret = key - strbyte("a") + 1
      break
    end
  end
  screen_load()

  return ret
end

-- Returns the name of the rune with the provided tval/sval.
function rune_name(tval, sval)
  -- There doesn't seem to be any easier way of doing this than to create
  -- a rune of the appropriate type and rip the relevant bit out of its
  -- name.
  local obj = create_object(tval, sval)
  local name = object_desc(obj, -1, 0)
  end_object(obj)
  return gsub(name, "Rune %[([%w ]+)%]", "%1")
end

-- Returns nil if any of the specified conditions are true.
RSC_0MANA = 1
RSC_SEE = 2
RSC_NOCONF = 4
RSC_MAGIC = 8
function runespell_check(flags)
  if bor(flags, RSC_0MANA) ~= 0 and player.csp <= 0 then
    msg_print("You have no mana!")
    return nil
  end

  if bor(flags, RSC_SEE) ~= 0 and (player.blind > 0 or no_lite() == TRUE) then
    msg_print("You cannot see!")
    return nil
  end

  if bor(flags, RSC_NOCONF) ~= 0 and player.confused > 0 then
    msg_print("You are too confused!")
    return nil
  end

  if bor(flags, RSC_MAGIC) ~= 0 then
    if player.antimagic > 0 then
      msg_print("Your anti-magic field disrupts any magic attempts.")
      return nil
    end
    if player.anti_magic == TRUE then
      msg_print("Your anti-magic shell disrupts any magic attempts.")
      return nil
    end
  end

  return not nil
end

-- Compute the power of the provided runespell.
function runespell_power(spell)
  -- The original runespell formulation was based on mana, and the base
  -- power was defined by P=37*sqrt(M+3)/10.  In the new formulation, we
  -- use "spell level" instead; the mana cost is defined by M=(L+1)^2-3,
  -- and power simplifies to P=37*(L+1)/10.  By "base power", we mean the Y
  -- in the XdY damage spec; X is defined as Y/3.
  local sides = 37 * (spell.level + 1) / 10
  local dice = sides/3
  if dice < 1 then dice = 1 end
  -- TODO:  maybe adjust this based on Runecraft or Spell-power skill...
  return dice, sides
end

-- Compute an extra difficulty parameter based on the provided runespell's
-- secondary effects.
function runespell_difficulty(spell)
  local power = 0

  if spell.damtype == GF_MISSILE then power = power + 2 end	-- [Element]
  if spell.damtype == GF_CHAOS then power = power + 2 end	-- [Chaos]
  if spell.damtype == GF_PSI then power = power + 1 end		-- [Mind]
  if spell.damtype == GF_STASIS then power = power + 1 end	-- [Holding]
  if spell.damtype == GF_GRAVITY then power = power + 2 end	-- [Gravity]
  if spell.damtype == GF_RAISE then power = power + 1 end	-- [Undeath]
  if spell.damtype == GF_MAKE_GLYPH then power = power + 1 end	-- [Protection]
  if spell.damtype == GF_CHARM then power = power + 1 end	-- [Charm]
  if spell.damtype == GF_STONE_WALL then power = power + 1 end	-- [Prison]

  if band(spell.effect, RUNE_POWER_SURGE) ~= 0 then power = power + 4 end
  if band(spell.effect, RUNE_ARMAGEDDON) ~= 0 then power = power + 3 end
  if band(spell.effect, RUNE_CLOUD) ~= 0 then power = power + 3 end
  if band(spell.effect, RUNE_SPHERE) ~= 0 then power = power + 2 end
  if band(spell.effect, RUNE_CURTAIN) ~= 0 then power = power + 2 end
  if band(spell.effect, RUNE_RAY) ~= 0 then power = power + 1 end
  return power
end

-- Compute the failure probability for the provided spell.
function runespell_fail_prob(spell)
  local diff = runespell_difficulty(spell)
  local _, power = runespell_power(spell)
  local dex_ind = player.stat_ind[A_DEX+1]

  -- Base fail rate depends on the spell's power and its use of "difficult"
  -- runes (the latter decreasingly so as Runecraft skill increases)...
  local fail = power + diff * (5 - get_skill_scale(SKILL_RUNECRAFT, 5))

  -- ... and is reduced based on your DEX...
  fail = fail - 3 * (adj_mag_stat[dex_ind] - 1)

  -- ... and on your skill level in Runecraft...
  fail = fail - 3 * (get_skill(SKILL_RUNECRAFT) - spell.level)

  -- ... subject to a DEX-based minimum fail rate, and a separate 5%
  -- minumum fail rate if you don't have Perfect Casting.
  local minfail = adj_mag_fail[dex_ind]
  if minfail < 5 and has_ability(AB_PERFECT_CASTING) == FALSE then minfail = 5 end
  if fail < minfail then fail = minfail end

  -- The fail rate is higher if you're stunned.
  if player.stun > 50 then fail = fail + 25
  elseif player.stun > 0 then fail = fail + 15
  end

  -- We'll be nice and give you a 5% chance of success in any case...
  if fail > 95 then fail = 95 end

  return fail
end

-- Compute the radius of the ball(s) generated by the provided spell.
function runespell_ball_radius(spell)
  local _, power = runespell_power(spell)
  local rad = power/8

  if rad < 1 then rad = 1 end
  if rad > 10 then rad = 10 end
  return rad
end

-- Compute the mana cost of the provided spell.
function runespell_mana_cost(spell)
  local mana = (spell.level + 1)*(spell.level + 1) - 3
  if spell.stone ~= nil then
    mana = (mana * 75)/100
  end
  if mana < 1 then mana = 1 end
  return mana
end

-- Print a description of everything about the provided spell that's
-- affected by its level.
function runespell_level_descr(spell)
  local mana = runespell_mana_cost(spell)
  local dice, sides = runespell_power(spell)
  local rad = "  "
  local balls = "     "
  local dur = "   "
  local fail = runespell_fail_prob(spell)

  if band(spell.effect, bor(RUNE_ARMAGEDDON, RUNE_SPHERE, RUNE_CLOUD)) ~= 0 then
    rad = format("%2d", runespell_ball_radius(spell))
  end
  if band(spell.effect, RUNE_ARMAGEDDON) ~= 0 then
    balls = format("%2d-%-2d", rad+1, rad+rad)
  end
  if band(spell.effect, RUNE_CURTAIN) ~= 0 then
    dur = format("%3d", 8 + spell.level/2)
  elseif band(spell.effect, RUNE_CLOUD) ~= 0 then
    dur = format("%3d", 5 + 3*spell.level)
  end

  return format("%3d %4d %2dd%-3d %2s %5s %3s  %2d%%",
		spell.level, mana, dice, sides, rad, balls, dur, fail)
end

-- List the memorized runespells.
function runespell_list_memory(pfx)
  local spells = {}
  local key
  local start = 1

  for code, spell in runespell_memory do
    if code ~= "nsp" and code ~= "nsav" then 
      local name = strsub(spell.name, 1, 30)
      local desc = runespell_level_descr(spell)

      if not pfx or strsub(code, 1, 1) == pfx then
	tinsert(spells, format("  %2s) %-30s %s", code, name, desc))
      end
    end
  end
  sort(spells)

  local title = "<Spc>/'-' to scroll                Lvl Mana Damage  R Balls Dur Fail"
  local rows = 21
  if getn(spells) < 20 then rows = 1+getn(spells) end

  screen_save()
  while not nil do
    display_list(1, 0, rows, 74, title, spells, start, -1, 0)
    key = inkey()

    if key == strbyte(" ") then
      if start + 20 <= getn(spells) then start = start + 20 end
    elseif key == strbyte("-") then
      if start >= 20 then start = start - 20 end
    else
      break;
    end
  end
  screen_load()
  return key
end

-- Item selector hook to choose primary runes.
function is_primary_rune(obj)
  if obj.tval == TV_RUNE1 then return TRUE end
  return FALSE
end

-- Item selector hook to choose secondary runes.  The 'used_runes' variable
-- allows us to restrict which secondary runes can be selected.
used_runes = 0
function is_secondary_rune(obj)
  if obj.tval ~= TV_RUNE2 or obj.sval == RUNE_STONE then return FALSE end
  if band(used_runes, lshift(1, obj.sval)) ~= 0 then return FALSE end
  return TRUE
end

-- Item selector hook to choose blank runestones.
function is_runestone(obj)
  if obj.tval == TV_RUNE2 and obj.sval == RUNE_STONE then return TRUE end
  return FALSE
end

-- Item selector hook to choose blank runestones.
function is_blank_runestone(obj)
  if obj.tval == TV_RUNE2 and obj.sval == RUNE_STONE and obj.pval == 0 then return TRUE end
  return FALSE
end

-- Item selector hook to choose runestones inscribed with a spell.
function is_nonblank_runestone(obj)
  if obj.tval == TV_RUNE2 and obj.sval == RUNE_STONE and obj.pval ~= 0 then return TRUE end
  return FALSE
end

-- Get a two-character code for a runespell.
function runespell_get_code(prompt)
  local c1 = "?"
  local get_key = not nil
  local ret, key

  while not nil do
--    prt(prompt ..  "[" .. c1 .. "?]: ", 0, 0)

    if get_key then ret, key = get_com(prompt .. "[" .. c1 .. "?]: ") end
    get_key = not nil
--    if key == ESCAPE then return nil end
    if ret == FALSE then return nil end
    if key == 127 or key == 8 then
      c1 = "?"
    elseif key == strbyte("?") then
      if c1 ~= "?" then
	key = runespell_list_memory(c1)
      else
	key = runespell_list_memory()
      end
      if key ~= strbyte("?") then
	get_key = nil
	ret = TRUE
      end
    elseif key > 32 and key < 127 then
      -- A valid code character.  If it's the second, return both; if it's
      -- the first, remember it and go back for the second.
      if c1 ~= "?" then return c1 .. strchar(key) end
      c1 = strchar(key)
    end
  end
end

-- Set a runespell's name based on its damage type, effect types, and
-- level.
function runespell_set_name(spell)
  local i
  local sep = "/"

  spell.name = rune_name(TV_RUNE1, spell.damtype)
  for i = 0, RUNE_MOD_MAX - 1 do
    if band(spell.effect, lshift(1, i)) ~= 0 then
      spell.name = spell.name .. sep .. rune_name(TV_RUNE2, i)
      sep = "+"
    end
  end
  spell.name = spell.name .. "/L" .. spell.level
end

-- Check if we have in inventory the runes necessary to cast the provided
-- runespell.
function runespell_has_runes(spell)
  local got_primary = nil
  local got_secondary = 0
  local i

  for i = 0, INVEN_WIELD - 1 do
    local obj = get_object(i)
    if obj.tval == TV_RUNE1 and obj.sval == spell.damtype then
      got_primary = not nil
    elseif obj.tval == TV_RUNE2 then
      got_secondary = bor(got_secondary, lshift(1, obj.sval))
    end
  end

  if got_primary and band(got_secondary, spell.effect) == spell.effect then
    return not nil
  end
  return nil
end

-- Create a runespell structure from runes selected from inventory.  The
-- optional 'stone' parameter is the stone onto which this spell is
-- destined to be inscribed; we pass it in because its mana-cost reduction
-- will influence the range of spell levels we can choose from.
function runespell_get_from_runes(stone)
  local ret, item, obj
  local spell = {}
  if stone ~= nil then spell.stone = stone.sval end

  -- First, get the primary rune for the damage type.
  ret, item = get_item("Use which primary rune?",
		       "You have no primary runes.",
		       bor(USE_INVEN, USE_EQUIP), is_primary_rune)
  if ret == FALSE then return nil end
  obj = get_object(item)
  spell.damtype = obj.sval;

  -- Now select secondary runes for effect type.
  used_runes = 0
  while not nil do
    local none = "You have no secondary runes."
    if used_runes ~= 0 then none = "You have no more secondary runes." end
    ret, item = get_item("Use which secondary rune?", none,
			 bor(USE_INVEN, USE_EQUIP), is_secondary_rune)
    if ret == TRUE then
      obj = get_object(item)
      used_runes = bor(used_runes, lshift(1, obj.sval))
    else
      -- If we've selected some secondary runes, we're presumably done
      -- selecting them; if we haven't, either we didn't have any or we
      -- changed our mind.
      if used_runes == 0 then return nil end
      break
    end
  end
  spell.effect = used_runes
  used_runes = 0

  -- Finally choose the spell level.  We arbitrarily cap displayed spell
  -- levels at 44 for screen-size purposes, but we allow as high a spell
  -- level as the player's current mana allows.
  if player.csp <= 0 then
    msg_print("No mana!")
    return nil
  end

  spell.level = 1
  while runespell_mana_cost(spell) <= player.csp do
    spell.level = spell.level + 1
  end
  local max = spell.level - 1
  local i

  screen_save()
  for i = 1, 44 do
    spell.level = i
    if i > max then break end

    local desc = runespell_level_descr(spell)
    local r = spell.level+1
    local c = 2

    if spell.level > 22 then r = r - 22; c = 40 end
    if r == 2 then prt("  Lvl Mana Damage  R Balls Dur Fail  ", 1, c) end
    prt("  " .. desc .. "  ", r, c)
  end
  spell.level = get_quantity("Spell level (1-" .. max .. "): ", max)
  screen_load()

  if spell.level <= 0 then return nil end

  -- Set the spell's name.
  runespell_set_name(spell)

  -- And we're done.
  return spell
end

-- Select a runespell from our list of memorized runespells.
function runespell_get_from_memory()
  if runespell_memory.nsp == 0 then
    msg_print("You have not memorized any runespells.");
    return nil
  else
    local code = runespell_get_code("Enter two-character code of runespell to cast")
    if code == nil or runespell_memory[code] == nil then return nil end
    return runespell_memory[code]
  end
end

-- Create a runespell structure corresponding to the runespell carved on
-- the provided runestone.
function runespell_get_from_runestone(obj)
  if obj.pval == 0 then return nil end
  local spell = {
    ["damtype"] = obj.pval,
    ["effect"]  = obj.pval2,
    ["level"]   = obj.pval3,
    ["stone"]   = obj.sval,
  }
  runespell_set_name(spell)
  return spell
end

-- Carve a runespell onto a runestone.
function runespell_make_runestone()
  local ret, item, obj, spell

  if not runespell_check(bor(RSC_SEE, RSC_NOCONF)) then return nil end

  if get_check("Beware, this will destroy the involved runes.  Continue?") == FALSE then return nil end

  -- First get the runestone to put a spell on.
  ret, item = get_item("Inscribe spell on which runestone?",
		       "You have no blank runestones.",
		       bor(USE_INVEN, USE_EQUIP), is_blank_runestone)
  if ret == FALSE then return nil end
  obj = get_object(item)

  -- Then get the spell to put on it.
  spell = runespell_get_from_runes(obj)
  if spell == nil then return nil end

  -- Then actually put it on the runestone.  The C code uses pval3 for the
  -- mana cost rather than the spell level, but what the C code doesn't
  -- know won't hurt us... ;)
  obj.pval = spell.damtype
  obj.pval2 = spell.effect
  obj.pval3 = spell.level

  -- Add an inscription to the runestone.
  local quark = ""
  if obj.note ~= 0 then quark = quark_str(obj.note) end
  if get_string("Name this runestone: ", quark, 80) == TRUE then
    obj.note = quark_add(quark)
    player.notice = bor(player.notice, PN_COMBINE)
    player.window = bor(player.window, PW_INVEN, PW_EQUIP)
  end

  -- Destroy the runes used.
  local i
  local did_del = 0

  for i = 0, INVEN_WIELD - 1 do
    obj = get_object(i)
    if obj.k_idx > 0 then
      local do_del = FALSE

      if obj.tval == TV_RUNE1 and obj.sval == spell.damtype then
	do_del = TRUE
      elseif obj.tval == TV_RUNE2 then
	local bit = lshift(1, obj.sval)
	if band(spell.effect, bit) ~= 0 and band(did_del, bit) == 0 then
	  do_del = TRUE
	  did_del = bor(did_del, bit)
	end
      end

      if do_del == TRUE then
	inven_item_increase(i, -1)
	inven_item_describe(i)
	-- Can't do this here; see below.
	--inven_item_optimize(i)
      end
    end
  end

  -- We can't do inven_item_optimize() _en_passant_ in the first loop,
  -- because that could cause things to shift around out from under us and
  -- we could miss runes.  Instead, we do them all here --- indexed
  -- backward, to avoid the same effect happening here.
  for i = INVEN_WIELD - 1, 0, -1 do
    inven_item_optimize(i)
  end
  -- Objects in inventory are sorted by decreasing tval, so all the runes
  -- come after the Runestone, which means we know the Runestone hasn't
  -- moved.
  inven_item_describe(item)

  return not nil
end

-- Cast a runespell.  Returns the amount of player time used to cast the
-- runespell.
function runespell_cast(spell)
  local mana = runespell_mana_cost(spell)

  -- In original runecraft, if we didn't have as much mana as was
  -- originally specified in the spell, the mana cost (and power) of the
  -- spell would be scaled down to the amount of mana we did have.  In the
  -- new formulation, mana and power are a function of level, and scaling
  -- level down with mana would require an integer square root function
  -- that I don't really want to bother with.  Instead, we just punt.
  if mana > player.csp then
    msg_print("You do not have enough mana.")
    return 0
  end

  local energy = 100
  local obj = get_object(INVEN_WIELD)
  if spell.stone ~= nil then energy = energy - get_skill(SKILL_RUNECRAFT) end
  if obj.k_idx > 0 and obj.tval == TV_MSTAFF then energy = (energy * 80)/100 end

  local dice, sides = runespell_power(spell)
  local dam = damroll(dice, sides)
  local fail = runespell_fail_prob(spell)

  if rand_int(100) < fail then
    -- We failed in casting the spell.  If we've lost sanity, give a cute
    -- failure message with probability proportionaly to how much sanity
    -- we've lost.
    local insanity = (player.msane - player.csane)*100/player.msane
    if rand_int(100) < insanity then
      msg_format("The runes rearrange themselves to spell, 'Go Stick Your Head In a Pig'.")
    else
      msg_format("You failed to get the spell off.")
    end

    player.window = bor(player.window, PW_PLAYER)
    player.redraw = bor(player.redraw, PR_MANA)
    player.csp = player.csp - mana
    return energy
  end

  -- Set up all the spell effect parameters.
  local flag = 0
  local ty = player.py
  local tx = player.px
  local rad = 0

  if (band(spell.effect, RUNE_POWER_SURGE) ~= 0) then
    flag = bor(flag, PROJECT_VIEWABLE)
  end
  if (band(spell.effect, RUNE_ARMAGEDDON) ~= 0) then
    flag = bor(flag, PROJECT_THRU, PROJECT_KILL, PROJECT_ITEM, PROJECT_GRID,
		     PROJECT_METEOR_SHOWER)
    rad = runespell_ball_radius(spell)
  end
  if (band(spell.effect, RUNE_CLOUD) ~= 0) then
    flag = bor(flag, PROJECT_THRU, PROJECT_KILL, PROJECT_ITEM, PROJECT_GRID, PROJECT_STAY)
    rad = runespell_ball_radius(spell)
    project_time = 5 + 3*spell.level
  end
  if (band(spell.effect, RUNE_SPHERE) ~= 0) then
    flag = bor(flag, PROJECT_THRU, PROJECT_KILL, PROJECT_ITEM, PROJECT_GRID)
    rad = runespell_ball_radius(spell)
  end
  if (band(spell.effect, RUNE_CURTAIN) ~= 0) then
    flag = bor(flag, PROJECT_THRU, PROJECT_KILL, PROJECT_ITEM, PROJECT_GRID, PROJECT_BEAM, PROJECT_STAY)
    ty = -1
    tx = -1
    project_time = 8 + spell.level/2
  end
  if (band(spell.effect, RUNE_RAY) ~= 0) then
    flag = bor(flag, PROJECT_THRU, PROJECT_KILL, PROJECT_BEAM)
    ty = -1
    tx = -1
  end
  if (band(spell.effect, RUNE_ARROW) ~= 0) then
    flag = bor(flag, PROJECT_THRU, PROJECT_STOP, PROJECT_KILL)
    ty = -1
    tx = -1
  end
  if (band(spell.effect, RUNE_SELF) ~= 0) then
    flag = bor(flag, PROJECT_THRU, PROJECT_STOP, PROJECT_KILL)
    -- TODO:  The code sets an 'unsafe' flag here, which AIUI tells
    -- project() that the spell can affect the player.  That flag doesn't
    -- seem to be accessible from Lua; we fake it by using -2 for the "who"
    -- parameter to project() instead of zero.
  end

  -- The GF_GRAVITY damage type keys off the 'unsafe' flag mentioned above,
  -- doing normal damage to monsters but only teleporting the player; we
  -- work around this by using the GF_SAFEGRAVITY damage type we defined
  -- above.
  local gf = spell.damtype
  if gf == GF_GRAVITY then gf = GF_SAFEGRAVITY end

  --- Aim...
  if ty == -1 and tx == -1 then
    local ret, dir = get_aim_dir()
    if ret == FALSE then return 0 end
    ty, tx = get_target(dir)
    
    -- Allow for shooting a ball/cloud into the middle of a group, or at a
    -- targeted point in the middle of a room.
    if dir == 5 and band(spell.effect, RUNE_ARROW) ~= 0 and rad > 0 then
      flag = band(flag, bnot(bor(PROJECT_STOP, PROJECT_THRU)))
    end
  end

  -- aaaand... fire!
  if band(flag, PROJECT_VIEWABLE) ~= 0 then
    project_los(gf, dam)
  elseif band(flag, PROJECT_METEOR_SHOWER) ~= 0 then
    project_meteor(rad, gf, dam, flag)
  elseif band(spell.effect, RUNE_SELF) ~= 0 then
    project(-2, rad, ty, tx, dam, gf, flag)
  else
    project(0, rad, ty, tx, dam, gf, flag)
  end

  -- Redraw stuff.
  player.window = bor(player.window, PW_PLAYER)
  player.redraw = bor(player.redraw, PR_MANA)
  player.csp = player.csp - mana
  return energy
end

-- The main dispatch function.
function new_runecraft()
  local op = runecraft_menu()
  local energy_needed = 100

  if op == 0 then return end
  if op == 1 then
    -- Cast from runes
    if runespell_check(bor(RSC_0MANA, RSC_SEE, RSC_NOCONF)) then
      local spell = runespell_get_from_runes()
      if spell == nil then
	energy_needed = 0
      else
	energy_needed = runespell_cast(spell)
      end
    else
      energy_needed = 0
    end

  elseif op == 2 then
    -- Cast memorized runespell
    if runespell_check(bor(RSC_0MANA, RSC_NOCONF, RSC_MAGIC)) then
      local spell = runespell_get_from_memory()
      if spell == nil then
	energy_needed = 0
      elseif not runespell_has_runes(spell) then
	msg_print("You don't have all the runes for this runespell!")
	energy_needed = 0
      else
	energy_needed = runespell_cast(spell)
      end
    else
      energy_needed = 0
    end

  elseif op == 3 then
    -- Memorize runespell
    if runespell_check(RSC_NOCONF) then
      local spell = runespell_get_from_runes()

      if spell ~= nil then
	local keep = nil
	local code = runespell_get_code("Enter two-character code for this runespell")

	if code ~= nil then
	  if runespell_memory[code] == nil then
	    runespell_memory[code] = spell
	    runespell_memory.nsp = runespell_memory.nsp + 1
	    if runespell_memory.nsav < runespell_memory.nsp then
	      -- Dodgy Hack(TM) [see below]:  We now have more spells
	      -- memorized than our reserved savefile slots can hold;
	      -- reserve enough additional slots for one more spell.
	      runespell_memory.nsav = runespell_memory.nsav + 1
	      register_savefile(3)
	    end
	    keep = not nil
	  elseif get_check("Replace runespell <" .. runespell_memory[code].name .. "> on code '" .. code .. "'?") == TRUE
	  then
	    runespell_memory[code] = spell
	    keep = not nil
	  end
	end
	if keep then
	  msg_print("Memorized runespell <" .. spell.name .. "> on code '" .. code .. "'.")
	end
      end
    end
    energy_needed = 0

  elseif op == 4 then
    -- Forget memorized runespell
    if runespell_check(RSC_NOCONF) then
      if runespell_memory.nsp == 0 then
	msg_print("You have not memorized any runespells.");
      else
	local code = runespell_get_code("Enter two-character code of runespell to be forgotten")
	if code ~= nil then
	  if runespell_memory[code] == nil then
	    msg_print("No memorized runespell at code '" .. code .. "'")
	  else
	    msg_print("Forgotten runespell <" .. runespell_memory[code].name .. "> at code '" .. code .. "'")
	    runespell_memory[code] = nil
	    runespell_memory.nsp = runespell_memory.nsp - 1
	  end
	end
      end
    end
    energy_needed = 0

  elseif op == 5 then
    -- List memorized runespells
    if runespell_memory.nsp == 0 then
      msg_print("You have not memorized any runespells.");
    else
      local _ = runespell_list_memory()
    end
    energy_needed = 0

  elseif op == 6 then
    -- Cast from Runestone
    if runespell_check(bor(RSC_0MANA, RSC_SEE, RSC_NOCONF, RSC_MAGIC)) then
      local spell = nil
      local ret, item = get_item("Cast from which runestone?",
			   "You have no inscribed runestones.",
			   bor(USE_INVEN, USE_EQUIP), is_nonblank_runestone)
      if ret == TRUE then
	spell = runespell_get_from_runestone(get_object(item))
      end
      if spell == nil then
	energy_needed = 0
      else
	energy_needed = runespell_cast(spell)
      end
    else
      energy_needed = 0
    end


  elseif op == 7 then
    -- Examine Runestone
    local ret, item = get_item("Examine which runestone?",
			  "You have no inscribed runestones.",
			  bor(USE_INVEN, USE_EQUIP), is_runestone)
    if ret == TRUE then
      local spell = runespell_get_from_runestone(get_object(item))
      if spell == nil then
	msg_print("This runestone is blank.")
      else
	local name = strsub(spell.name, 1, 30)
	local desc = runespell_level_descr(spell)
	local line = format("%-30s %s", name, desc)
	local spells = { line }
	local title = "Spell                        Lvl Mana Damage  R Balls Dur Fail"

	screen_save()
	display_list(1, 0, 2, 68, title, spells, 1, -1, 0)
	local _ = inkey()
	screen_load()
      end

      energy_needed = 0
    end

  elseif op == 8 then
    -- Carve runespell onto Runestone
    if runespell_make_runestone() then
      -- Carving a runestone takes a long time.
      energy_needed = 400
    else
      energy_needed = 0
    end

  else
    msg_print("??? Runecraft op " .. op)
    energy_needed = 0
  end

  energy_use = energy_use + energy_needed
end


-- Dodgy Hack(TM):  ToME's built-in savefile extension mechanism is based
-- on saving and load a predetermined number of key/number pairs; the
-- add_loadsave() mechanism built on top of it is predicated on the
-- strategy of saving and loading tables of fixed size and arrangement.
-- That's not sufficient for our needs, since our table of memorized
-- runespells can grow and shrink in-game, and its fields will depend on
-- the spell codes the user enters.  Thus we use our own load/save-game
-- hooks to break the spells out into key/number pairs to save and
-- reassemble them on loading.  The tricky part is setting the number of
-- reserved key/number pairs; ideally we'd like to do this just before
-- saving, but we can't get control in time, so we have to adjust it on the
-- fly.

-- Our custom load-from-savefile hook.
function runecraft_load_memory(key, val)
  -- If the key doesn't have our special prefix, it's not for us.
  if strsub(key, 1, 6) ~= "@Rune:" then return end

  -- A dummy padding key, explained below
  if key == "@Rune:nil" then return end

  -- Skip the prefix.
  key = strsub(key, 7)

  if key == "nsp" then
    -- The number of memorized spells in the savefile.
    runespell_memory.nsp = val
    runespell_memory.nsav = 0
    return
  end
  if strsub(key, 3, 3) == "." then
    -- A field of one of the spells, in the form "cc.fieldname", where 'cc'
    -- is the user's assigned code.
    local code = strsub(key, 1, 2)
    local field = strsub(key, 4)

    if not runespell_memory[code] then
      -- If the spell doesn't already exist, create it, and count the
      -- number of spells we've created.
      runespell_memory[code] = {}
      runespell_memory.nsav = runespell_memory.nsav + 1
    end
    -- Add the field to the spell table.
    runespell_memory[code][field] = val
    if field == "level" then
      -- This is the last field of each spell.  Now that we've finished
      -- pupulating it, we can set its name.
      runespell_set_name(runespell_memory[code])
      if runespell_memory.nsav == runespell_memory.nsp then
	-- This is (or should be) the last memorized spell.  Reserve enough
	-- key/number slots to save them all back out (including the one we
	-- reserved way back up at the top for the number of runespells.
	register_savefile(3*runespell_memory.nsp)
      end
    end
  end
end

-- Our custom save-to-savefile hook.
function runecraft_save_memory()
  local code, spell, i

  -- First, save the number of spells.
  save_number_key("@Rune:nsp", runespell_memory.nsp)
  -- Then save each spell.
  for code, spell in runespell_memory do
    if code ~= "nsp" and code ~= "nsav" then
      save_number_key("@Rune:" .. code .. ".damtype", spell.damtype)
      save_number_key("@Rune:" .. code .. ".effect", spell.effect)
      save_number_key("@Rune:" .. code .. ".level", spell.level)
    end
  end

  -- If we've got more reserved key/number slots than we need for our
  -- spells, pad them out with dummy padding.
  for i = runespell_memory.nsp + 1, runespell_memory.nsav do
    save_number_key("@Rune:nil", 0)
    save_number_key("@Rune:nil", 0)
    save_number_key("@Rune:nil", 0)
  end
end

add_hook_script(HOOK_LOAD_GAME, "runecraft_load_memory", "runecraft_load_memory")
add_hook_script(HOOK_SAVE_GAME, "runecraft_save_memory", "runecraft_save_memory")


-- New entry on the 'm' menu.
MKEY_RUNECRAFT_NEW = 1042
add_mkey
{
  ["mkey"] = MKEY_RUNECRAFT_NEW,
  ["fct"] = new_runecraft,
}
