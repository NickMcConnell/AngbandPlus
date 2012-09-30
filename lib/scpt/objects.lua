-- SYSTEM FILE
--
-- Lua object funtions
--

function create_object(tval, sval)
	local obj = new_object()
        object_prep(obj, lookup_kind(tval, sval))
        return (obj)
end

function set_item_tester(tester)
	if tolua.type(tester) == "number" then
		lua_set_item_tester(tester, "")
        end
	if tolua.type(tester) == "string" then
		lua_set_item_tester(0, tester)
        end
end
