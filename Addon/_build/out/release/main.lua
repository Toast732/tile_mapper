 
--[[
	
Copyright 2023 Liam Matthews

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

]]

--! (If gotten from Steam Workshop) LICENSE is in vehicle_0.xml
--! (If gotten from anywhere else) LICENSE is in LICENSE and vehicle_0.xml

ADDON_VERSION = "(0.1.0.0)"

-- g_savedata table that persists between game sessions
g_savedata = {
	ui_id = nil,
	tiles = {},
	tiles_to_draw = {},
	drawing_pos = 0,
	tiles_to_remove = 0,
	removing_pos = 0,
	tile_ui_ids = {}
}

local map_bounds = {
	x = {
		min = -65500,
		max = 64500
	},
	z = {
		min = -63500,
		max = 125500
	}
}

local tile_length = 1000

local drawn_per_tick = 50

function onCreate()
	if not g_savedata.removing_pos then
		g_savedata.removing_pos = 0
		g_savedata.tile_ui_ids = {}
		g_savedata.tiles_to_remove = 0
	end
	
	if type(g_savedata.tiles_to_remove) == "table" then
		g_savedata.tiles_to_remove = #g_savedata.tile_ui_ids
	end
end

function onTick()
	if g_savedata.removing_pos and g_savedata.removing_pos > 0 then
		for i = g_savedata.removing_pos, g_savedata.removing_pos + drawn_per_tick do
			g_savedata.removing_pos = g_savedata.removing_pos + 1
			
			if i > g_savedata.tiles_to_remove then
				g_savedata.removing_pos = 0
				g_savedata.tiles_to_remove = 0
				break
			end
			
			server.removeMapID(-1, g_savedata.tile_ui_ids[i])
		end
	elseif g_savedata.drawing_pos and g_savedata.drawing_pos > 0 then
		--local start_time = server.getTimeMillisec()
		for i = g_savedata.drawing_pos, g_savedata.drawing_pos - drawn_per_tick, -1 do
		
			g_savedata.drawing_pos = g_savedata.drawing_pos - 1
			
			if i <= 0 then
				break
			end
			
			g_savedata.tile_ui_ids[i] = g_savedata.tile_ui_ids[i] or server.getMapID()
			local ui_id = g_savedata.tile_ui_ids[i]
		
			local name = g_savedata.tiles_to_draw[i].name
			local x = g_savedata.tiles_to_draw[i].transform[13] + 50
			local z = g_savedata.tiles_to_draw[i].transform[15] + 950
		
			server.addMapLabel(-1, ui_id, 1, name, x, z)
			
			g_savedata.tiles_to_remove = g_savedata.tiles_to_remove + 1
			
			-- if we've been adding these for over 1ms
			--[[if server.getTimeMillisec() - start_time > 16.6667 then
				break
			end]]
		end
	end
end

function onCustomCommand(full_message, peer_id, is_admin, is_auth, prefix, command, ...)

	prefix = string.lower(prefix)

	--? if the command they're entering is not for this addon
	if prefix ~= "?tm" then
		return
	end

	--? if they didn't enter a command
	if not command then
		return
	end

	--*---
	--* handle the command the player entered
	--*---
	
	if command == "draw" then
		if not g_savedata.ui_id then
			g_savedata.ui_id = server.getMapID()
		else
			server.removeMapID(-1, g_savedata.ui_id)
		end
		
		-- draw grid lines
		
		-- x axis grid lines
		for x = map_bounds.x.min, map_bounds.x.max, tile_length do
			server.addMapLine(peer_id, g_savedata.ui_id, matrix.translation(x, 0, map_bounds.z.min), matrix.translation(x, 0, map_bounds.z.max), 0.75, 85, 85, 85, 255)
		end
		
		-- z axis grid lines
		for z = map_bounds.z.min, map_bounds.z.max, tile_length do
			server.addMapLine(peer_id, g_savedata.ui_id, matrix.translation(map_bounds.x.min, 0, z), matrix.translation(map_bounds.x.max, 0, z), 0.75, 85, 85, 85, 255)
		end
		
		g_savedata.removing_pos = 1
		
		g_savedata.tiles = {}
		
		g_savedata.tiles_to_draw = {}
		
		-- go through all tiles
		
		local player_pos = server.getPlayerPos(peer_id)
		
		-- x axis
		for x = map_bounds.x.min, map_bounds.x.max - tile_length + 1, tile_length do
			-- z axis
			for z = map_bounds.z.min, map_bounds.z.max - tile_length + 1, tile_length do
				local tile_transform = matrix.translation(x, 0, z)
				local tile_data = server.getTile(tile_transform)
				if tile_data then
					-- put in top left corner
					
					if tile_data.name == nil or tile_data.name == "" then
						tile_data.name = "ocean"
					else
						tile_data.name = tile_data.name:gsub("(.*)/(.*)%.xml", "%2")
					end
					
					
					
					g_savedata.tiles[x] = g_savedata.tiles[x] or {}
					g_savedata.tiles[x][z] = {
						data = tile_data,
						transform = tile_transform
					}
					
					table.insert(g_savedata.tiles_to_draw, {
						transform = tile_transform,
						player_dist = math.euclideanDistance(x, player_pos[13], z, player_pos[15]),
						name = tile_data.name
					})
				end
			end
		end
		
		-- sort tiles to draw from closest to player to furthest from player
		table.sort(
			g_savedata.tiles_to_draw, 
			function(a, b)
				return a.player_dist > b.player_dist
			end
		)
		
		g_savedata.drawing_pos = #g_savedata.tiles_to_draw
		
		
	elseif command == "remove" then
		if g_savedata.ui_id then
			server.removeMapID(-1, g_savedata.ui_id)
		end
		
		g_savedata.removing_pos = 1
	end
end
	
---@param x1 number x coordinate of position 1
---@param x2 number x coordinate of position 2
---@param z1 number z coordinate of position 1
---@param z2 number z coordinate of position 2
---@param y1 number? y coordinate of position 1 (exclude for 2D distance, include for 3D distance)
---@param y2 number? y coordinate of position 2 (exclude for 2D distance, include for 3D distance)
---@return number distance the euclidean distance between position 1 and position 2
function math.euclideanDistance(...)
	local c = table.pack(...)

	local rx = c[1] - c[2]
	local rz = c[3] - c[4]

	if #c == 4 then
		-- 2D distance
		return math.sqrt(rx*rx+rz*rz)
	else
		-- 3D distance
		local ry = c[5] - c[6]
		return math.sqrt(rx*rx+ry*ry+rz*rz)
	end
end

