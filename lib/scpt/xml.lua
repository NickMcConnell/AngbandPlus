-- The xml module
xml = {}

function xml:parseargs (s)
  local arg = {}
  gsub(s, "(%w+)=([\"'])(.-)%2", function (w, _, a)
    %arg[w] = a
  end)
  return arg
end

-- s is a xml stream, returns a table
function xml:collect (s)
  local stack = {n=0}
  local top = {n=0}
  tinsert(stack, top)
  local ni,c,label,args, empty
  local i, j = 1, 1
  while 1 do
    ni,j,c,label,args, empty = strfind(s, "<(%/?)(%w+)(.-)(%/?)>", j)
    if not ni then break end
    local text = strsub(s, i, ni-1)
    if not strfind(text, "^%s*$") then
      tinsert(top, text)
    end
    if empty == "/" then  -- empty element tag
      tinsert(top, {n=0, label=label, args=xml:parseargs(args), empty=1})
    elseif c == "" then   -- start tag
      top = {n=0, label=label, args=xml:parseargs(args)}
      tinsert(stack, top)   -- new level
    else  -- end tag
      local toclose = tremove(stack)  -- remove top
      top = stack[stack.n]
      if stack.n < 1 then
        error("nothing to close with "..label)
      end
      if toclose.label ~= label then
        error("trying to close "..toclose.label.." with "..label)
      end
      tinsert(top, toclose)
    end 
    i = j+1
  end
  local text = strsub(s, i)
  if not strfind(text, "^%s*$") then
    tinsert(stack[stack.n], text)
  end
  if stack.n > 1 then
    error("unclosed "..stack[stack.n].label)
  end
  return stack[1]
end

-- Viewport coordinates
xml.write_out_y = 0
xml.write_out_x = 0
xml.write_out_h = 24
xml.write_out_w = 80

-- Offsets
xml.write_off_y = 0
xml.write_off_x = 0

-- Current position
xml.write_y = 0
xml.write_x = 0

xml.write_screen = function(color, s)
        local i
        for i = 1, strlen(s) do
                local c = strsub(s, i, i + 1)
                if c ~= "\n" then
                        if xml.write_y - xml.write_off_y >= 0 and xml.write_y - xml.write_off_y < xml.write_out_h and xml.write_x - xml.write_off_x >= 0 and xml.write_x - xml.write_off_x < xml.write_out_w then
		                Term_putch(xml.write_x - xml.write_off_x + xml.write_out_x, xml.write_y - xml.write_off_y + xml.write_out_y, color, strbyte(c))
                        end
		        xml.write_x = xml.write_x + 1
                else
		        xml.write_x = 0
		        xml.write_y = xml.write_y + 1
                end
        end
end

xml.write_file = function (color, s)
        print_hook(s)
end

xml.write = xml.write_screen

function xml:print_xml(t, tab)
	local i, k, e
        local inside = nil
        local bcol, ecol = TERM_L_GREEN, TERM_GREEN

        if xml.write_active and t == auto_aux.rule then bcol, ecol = TERM_VIOLET, TERM_VIOLET end

        xml.write(bcol, tab.."<"..t.label)
        for k, e in t.args do
        	xml.write(TERM_L_BLUE, " "..k)
        	xml.write(TERM_WHITE, "=\"")
        	xml.write(TERM_YELLOW, e)
        	xml.write(TERM_WHITE, "\"")
        end
        xml.write(bcol, ">")

        for i = 1, getn(t) do
                if type(t[i]) == "string" then
                        xml.write(TERM_WHITE, t[i])
                else
                	if not inside then xml.write(TERM_WHITE, "\n") end
                        inside = not nil
                        xml:print_xml(t[i], tab.."    ")
                end
        end

        if not inside then
	        xml.write(ecol, "</"..t.label..">\n")
        else
	        xml.write(ecol, tab.."</"..t.label..">\n")
        end
end

-- t is a table representing xml, outputs the xml code via xml.write()
function xml:output(t)
        local i
        for i = 1, getn(t) do
	        xml:print_xml(t[i], "")
        end
end
