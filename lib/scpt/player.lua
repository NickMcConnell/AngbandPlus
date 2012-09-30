-- SYSTEM FILE
--
-- Lua player funtions
--

player = p_ptr

-- modify mana
-- returns TRUE if there is a pb
function increase_mana(amt)
        p_ptr.csp = p_ptr.csp + amt
        p_ptr.redraw = bor(p_ptr.redraw, PR_MANA)
        if (p_ptr.csp < 0) then
                p_ptr.csp = 0
                return TRUE
        end
        return FALSE
end
