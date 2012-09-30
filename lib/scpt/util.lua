-- various stuff to make scripters life easier

-- Beware of the scary undefined globals
function safe_getglobal(x)
        local v = rawget(globals(), x)

        if v then
                return v
        else
                error("undefined global variable '"..x.."'")
        end
end

function set_safe_globals()
        settagmethod(tag(nil), "getglobal", safe_getglobal)
end
function unset_safe_globals()
        settagmethod(tag(nil), "getglobal", nil)
end

set_safe_globals()

-- Patch modules
__patch_modules = {}

function patch_version(name, version)
        assert(not __patch_modules[name], "Patch " .. name .. " already loaded!!!")
        __patch_modules[name] = version
end

function patchs_list()
        local k, e, first
        first = FALSE
        for k, e in __patch_modules do
                if first == FALSE then print_hook("\n\n  [Patch modules]\n") first = TRUE end
                print_hook("\n "..k.." version "..e)
        end
        if first == TRUE then print_hook("\n") end
end


-- Better hook interface
__hooks_list_callback = {}
__hooks_list_callback_max = 0

function add_hooks(h_table)
        local k, e

        for k, e in h_table do
                add_hook_script(k, "__hooks_list_callback"..__hooks_list_callback_max, "__hooks_list_callback"..__hooks_list_callback_max)
                setglobal("__hooks_list_callback"..__hooks_list_callback_max, e)
                __hooks_list_callback_max = __hooks_list_callback_max + 1
        end
end

-- Wrapper for the real msg_print and cmsg_print
-- it understands if we want color or not
function msg_print(c, m)
        if type(c) == "number" then
                cmsg_print(c, m)
        else
                call(%msg_print, { c })
        end
end

-- Returns the direction of the compass that y2, x2 is from y, x
-- the return value will be one of the following: north, south, 
-- east, west, north-east, south-east, south-west, north-west
function compass(y, x, y2, x2)
        local y_axis, x_axis

        -- determine if y2, x2 is to the north or south of y, x
        if y2 > y then
                y_axis = "south"
        elseif y2 < y then
                y_axis = "north"
        else
                y_axis = nil
        end
                                                                
        -- determine if y2, x2 is to the east or west of y, x
        if x2 > x then
                x_axis = "east"
        elseif x2 < x then
                x_axis = "west"
        else
                x_axis = nil
        end
                
        -- Maybe it is due N/S
        if not x_axis then compass_dir = y_axis
        
        -- Maybe it is due E/W
        elseif not y_axis then compass_dir = x_axis
        
        -- or if it is neither
        else compass_dir = y_axis.."-"..x_axis
        end
                
        return compass_dir 
end
