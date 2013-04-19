function draw_we_road(seed)

    cave(32,0).mimic = 200
    cave(33,0).mimic = 200
    cave(32,197).mimic = 200
    cave(33,197).mimic = 200

    local random = imod(seed, 3)
    local seed2 = seed/10
    
    if (random == 0) then
        draw_h_snake(seed2)
    elseif (random == 1) then
        draw_h_turtle(seed2)
    else
        draw_h_camel(seed2)
    end

end

function draw_h_snake(seed)

    -- formula: y = x (x^2 -1) (x^2 -1), -1 < x < 1
    local i, j, A, B, cnt, random
    cnt = 40
    random = imod(seed, 2)*2 -1
    
    for i = 1, 195 do
        A = (cnt*(i -99))/99

        B = (A*A -cnt*cnt) * (A*A -cnt*cnt) * A
        j = 33 - random*(33*B)/(cnt*cnt*cnt*cnt*cnt)

        cave(j, i).feat = 200
        cave(j-1, i).feat = 200
        cave(j, i+1).feat = 200
        cave(j-1, i+1).feat = 200

--        cmsg_print(10, format("Warning! j = %s, i = %s", j, i))

    end
end

function draw_h_turtle(seed)

    -- formula: y = (x^2 -1) (x^2 -1), -1 < x < 1
    local i, j, A, B, cnt, random
    cnt = 10
    random = imod(seed, 2)*2 -1
    
    for i = 1, 195 do
        A = (cnt*(i -99))/99

        B = (A*A -cnt*cnt) * (A*A -cnt*cnt)
        j = 33 - random*(11*B)/(cnt*cnt*cnt*cnt)

        cave(j, i).feat = 200
        cave(j-1, i).feat = 200
        cave(j, i+1).feat = 200
        cave(j-1, i+1).feat = 200

--        cmsg_print(10, format("Warning! j = %s, i = %s", j, i))

    end
end

function draw_h_camel(seed)

    -- formula: y = (x^2 -1) (x^2 -1), -1 < x < 1
    local i, j, A, B, cnt, random
    cnt = 20
    random = imod(seed, 2)*2 -1
    
    for i = 1, 98 do
        A = (cnt*(i -50))/49

        B = (A*A -cnt*cnt) * (A*A -cnt*cnt)
        j = 33 - random*(11*B)/(cnt*cnt*cnt*cnt)

        cave(j, i).feat = 200
        cave(j-1, i).feat = 200
        cave(j, i+1).feat = 200
        cave(j-1, i+1).feat = 200

--        cmsg_print(10, format("Warning! j = %s, i = %s", j, i))

    end

    for i = 99, 195 do
        A = (cnt*(i -149))/49

        B = (A*A -cnt*cnt) * (A*A -cnt*cnt)
        j = 33 - random*(11*B)/(cnt*cnt*cnt*cnt)

        cave(j, i).feat = 200
        cave(j-1, i).feat = 200
        cave(j, i+1).feat = 200
        cave(j-1, i+1).feat = 200

--        cmsg_print(10, format("Warning! j = %s, i = %s", j, i))

    end
end

function draw_ns_road(seed)

    cave(0,98).mimic = 200
    cave(0,99).mimic = 200
    cave(65,98).mimic = 200
    cave(65,99).mimic = 200

    local random = imod(seed, 2)
    local seed2 = seed/10

    if (random == 0) then
        draw_v_snake(seed2)
    else
        draw_v_turtle(seed2)
    end

end

function draw_v_snake(seed)

    -- formula: x = y (y^2 -1) (y^2 -1), -1 < y < 1
    local i, j, A, B, cnt, random
    cnt = 50
    random = imod(seed, 2)*2 -1
    
    for j = 1, 63 do
        B = (cnt*(j -33))/33

        A = (B*B -cnt*cnt) * (B*B -cnt*cnt) * B
        i = 99 + random*(22*A)/(cnt*cnt*cnt*cnt*cnt)

        cave(j, i).feat = 200
        cave(j+1, i).feat = 200
        cave(j, i-1).feat = 200
        cave(j+1, i-1).feat = 200

--        cmsg_print(10, format("Warning! j = %s, i = %s", j, i))

    end
end

function draw_v_turtle(seed)

    -- formula: x = (y^2 -1) (y^2 -1), -1 < y < 1
    local i, j, A, B, cnt, random
    cnt = 20
    random = imod(seed, 2)*2 -1
    
    for j = 1, 63 do
        B = (cnt*(j -33))/33

        A = (B*B -cnt*cnt) * (B*B -cnt*cnt)
        i = 99 - random*(7*A)/(cnt*cnt*cnt*cnt)

        cave(j, i).feat = 200
        cave(j+1, i).feat = 200
        cave(j, i-1).feat = 200
        cave(j+1, i-1).feat = 200

--        cmsg_print(10, format("Warning! j = %s, i = %s", j, i))

    end
end

function draw_ne_road()

    cave(0,98).mimic = 200
    cave(0,99).mimic = 200
    cave(32,197).mimic = 200
    cave(33,197).mimic = 200

    -- formula: x = 1/y^2
    local i, j, A, cnt
    local lap, i_prev, j_prev, dif_i, dif_j
    cnt = 20
    A = 89*cnt
    lap = cnt
    i_prev = 198
    j_prev = 33

    while A > cnt do
        i = 196 - ((99 - A /cnt)*(99 - A /cnt))/98
        j = 33 - (33*cnt)/A

        dif_i = i -i_prev
        dif_j = j -j_prev

        if ((dif_i*dif_i < 9) and (dif_j*dif_j  < 9) and (dif_i*dif_i*dif_j*dif_j < 4)) or (lap==1) then

            i_prev = i
            j_prev = j

            cave(j, i).feat = 200
            cave(j-1, i).feat = 200
            cave(j, i+1).feat = 200
            cave(j-1, i+1).feat = 200
                
--            cmsg_print(10, format("Warning! A = %s, j = %s, i = %s", A/cnt, j, i))
                
        else
            A = A +lap
            lap = lap -1

        end
            A = A -lap
    end

end


function draw_nw_road()

    cave(0,98).mimic = 200
    cave(0,99).mimic = 200
    cave(32,0).mimic = 200
    cave(33,0).mimic = 200

    -- formula: x = 1/y^2
    local i, j, A, cnt
    local lap, i_prev, j_prev, dif_i, dif_j
    cnt = 20
    A = 89*cnt
    lap = cnt
    i_prev = 1
    j_prev = 33

    while A > cnt do
        i = ((99 - A /cnt)*(99 - A /cnt))/98
        j = 33 - (33*cnt)/A

        dif_i = i -i_prev
        dif_j = j -j_prev

        if ((dif_i*dif_i < 9) and (dif_j*dif_j  < 9) and (dif_i*dif_i*dif_j*dif_j < 4)) or (lap==1) then

            i_prev = i
            j_prev = j

            cave(j, i).feat = 200
            cave(j-1, i).feat = 200
            cave(j, i+1).feat = 200
            cave(j-1, i+1).feat = 200
                
--            cmsg_print(10, format("Warning! A = %s, j = %s, i = %s", A/cnt, j, i))
                
        else
            A = A +lap
            lap = lap -1

        end
            A = A -lap
    end

end

function draw_se_road()

    cave(65,98).mimic = 200
    cave(65,99).mimic = 200
    cave(32,197).mimic = 200
    cave(33,197).mimic = 200

    -- formula: x = 1/y^2
    local i, j, A, cnt
    local lap, i_prev, j_prev, dif_i, dif_j
    cnt = 20
    A = 89*cnt
    lap = cnt
    i_prev = 198
    j_prev = 33

    while A > cnt do
        i = 196 - ((99 - A /cnt)*(99 - A /cnt))/98
        j = 33 + (33*cnt)/A

        dif_i = i -i_prev
        dif_j = j -j_prev

        if ((dif_i*dif_i < 9) and (dif_j*dif_j  < 9) and (dif_i*dif_i*dif_j*dif_j < 4)) or (lap==1) then

            i_prev = i
            j_prev = j

            cave(j, i).feat = 200
            cave(j-1, i).feat = 200
            cave(j, i+1).feat = 200
            cave(j-1, i+1).feat = 200
                
--            cmsg_print(10, format("Warning! A = %s, j = %s, i = %s", A/cnt, j, i))
                
        else
            A = A +lap
            lap = lap -1

        end
            A = A -lap
    end

end



function draw_sw_road()

    cave(32,0).mimic = 200
    cave(33,0).mimic = 200
    cave(65,98).mimic = 200
    cave(65,99).mimic = 200

    -- formula: x = 1/y^2
    local i, j, A, cnt
    local lap, i_prev, j_prev, dif_i, dif_j
    cnt = 20
    A = 89*cnt
    lap = cnt
    i_prev = 1
    j_prev = 33
    total = 0
    while A > cnt do
        i = ((99 - A /cnt)*(99 - A /cnt))/98
        j = 33 + (33*cnt)/A

        dif_i = i -i_prev
        dif_j = j -j_prev

        if ((dif_i*dif_i < 9) and (dif_j*dif_j  < 9) and (dif_i*dif_i*dif_j*dif_j < 4)) or (lap==1) then

            i_prev = i
            j_prev = j

            cave(j, i).feat = 200
            cave(j-1, i).feat = 200
            cave(j, i+1).feat = 200
            cave(j-1, i+1).feat = 200
                
--            cmsg_print(10, format("Warning! A = %s, j = %s, i = %s", A/cnt, j, i))
                
        else
            A = A +lap
            lap = lap -1

        end
            A = A -lap
    end

end


function bs_draw_a_road()

    -- we aren't in the small map, are we?
    if (player.wild_mode == FALSE) then
    -- no, we aren't

        -- are we on a road?
        -- what kind of road?
        local y = player.wilderness_y
        local x = player.wilderness_x

        local wild_feat = wild_map(y, x).feat
        local wild_seed = wild_map(y, x).seed
        
        if (wild_feat == 41) then
            draw_ne_road()
        elseif (wild_feat == 42) then
            draw_ns_road(wild_seed)
        elseif (wild_feat == 43) then
            draw_nw_road()
        elseif (wild_feat == 44) then
            draw_we_road(wild_seed)
        elseif (wild_feat == 47) then
            draw_se_road()
        elseif (wild_feat == 49) then
            draw_sw_road()
        end
               
    end
end



add_hook_script(HOOK_WILD_GEN, "bs_draw_a_road", "bs_draw_a_road")
