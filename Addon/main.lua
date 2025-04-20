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

ADDON_VERSION = "(0.1.1.0)"

---@alias DrawType
---| "tile_grid"
---| "tile"
---| "env_zone"
---| "env_graph_node"

---@class DrawObject
---@field vehicles table<integer, integer> the vehicles associated with this drawn object
---@field main_ui_id SWUI_ID the ui_id for drawing the lines and labels to the map.
---@field draw_type DrawType the draw type.

---@class Colour
---@field r number red (0-255)
---@field g number green (0-255)
---@field b number blue (0-255)
---@field a number alpha (0-255)

---@class LineToDraw
---@field ui_type "line"
---@field draw_type DrawType the draw type of the object, such as "tile_grid"
---@field start_matrix SWMatrix the start position of the line
---@field end_matrix SWMatrix the end position of the line
---@field transform SWMatrix the middle of the line
---@field w number the width of the line
---@field colour Colour the colour of the line

---@class LabelToDraw
---@field ui_type "label"
---@field draw_type DrawType the draw type of the object, such as "tile_grid"
---@field label_type SWLabelTypeEnum the label type
---@field name string the text for the label
---@field transform SWMatrix the position of the label

---@class PolygonToDraw
---@field ui_type "polygon"
---@field draw_type DrawType the draw type of the object, such as "tile_grid"
---@field transform SWMatrix the position of the polygon
---@field width number the width of the polygon
---@field height number the height of the polygon
---@field sides integer the number of sides the polygon has
---@field rotation number the rotation of the polygon
---@field line_thickness number the thickness of the lines
---@field colour Colour the colour of the polygon

---@class objectsToDraw
---@field tile_grid table<integer, LineToDraw>
---@field tile table<integer, LabelToDraw>
---@field env_zone table<integer, PolygonToDraw>

local t ---@type SWAddonComponentData

---@class TMComponentData
---@field character_outfit_type SWOutfitTypeEnum the character's outfit type
---@field display_name string the component's display name
---@field dynamic_object_type SWObjectTypeEnum the object type
---@field id number the component's id
---@field tags table<integer, string> the tags of this component.
---@field tags_full string the tags as one continuous string
---@field transform SWMatrix the matrix of the component in the location
---@field type SWAddonComponentDataTypeEnum the type of the component
---@field vehicle_parent_component_id integer the vehicle component id which this component is parented to.
---@field location_data SWLocationData the location's data, which this component is in.
---@field addon_data SWAddonData the addon's data, which this component is in.

---@alias TMSpecificComponentList table<integer, TMComponentData>

---@class TMComponentList
---@field character TMSpecificComponentList
---@field object TMSpecificComponentList
---@field zone table<integer, SWZone>
---@field vehicle TMSpecificComponentList
---@field flare TMSpecificComponentList
---@field fire TMSpecificComponentList
---@field loot TMSpecificComponentList
---@field animal TMSpecificComponentList
---@field creature TMSpecificComponentList
---@field graph_node TMSpecificComponentList

-- g_savedata table that persists between game sessions
g_savedata = {
	tick_counter = 0,
	ui_id = nil,
	drawing = {
		drawn = {},
		drawing = {}, ---@type table<string, table<integer, LineToDraw|LabelToDraw|PolygonToDraw>>
		removing = {},
		next_draw_object_id = 1
	},
	tiles = {},
	tiles_to_draw = {},
	drawing_pos = 0,
	tiles_to_remove = 0,
	removing_pos = 0,
	tile_ui_ids = {},
	components = {
		character = {},
		object = {},
		zone = {},
		vehicle = {},
		flare = {},
		fire = {},
		loot = {},
		animal = {},
		creature = {},
		graph_node = {}
	},
	location_data = {},
	addon_data = {},
	libraries = {}
}

require("libraries.utils.math")
require("libraries.utils.table")

require("libraries.addon.script.matrix")

require("libraries.addon.ui.ui.ui") -- its nice for how it works, but the require paths... I need to stop looking at it.
require("libraries.addon.ui.ui_id.ui_id")

local map_bounds = {
	scotland = {
		x = {
			min = -65500,
			max = 64500
		},
		z = {
			min = -63500,
			max = 126500
		}
	},
	moon = {
		x = {
			min = 183500,
			max = 215500
		},
		z = {
			min = -16500,
			max = 15500
		}
	}
}

local tile_length = 1000

local actions_per_tick = 60

function INFO(msg)
	server.announce("Tile Mapper ", msg, -1)
	debug.log("SW TM |"..msg:gsub("\n", "\\n"))
end

---@type TMComponentList
local components = {
	character = {},
	object = {},
	zone = {},
	vehicle = {},
	flare = {},
	fire = {},
	loot = {},
	animal = {},
	creature = {},
	graph_node = {}
}

function onCreate()
	if not g_savedata.removing_pos then
		g_savedata.removing_pos = 0
		g_savedata.tile_ui_ids = {}
		g_savedata.tiles_to_remove = 0
	end
	
	if type(g_savedata.tiles_to_remove) == "table" then
		g_savedata.tiles_to_remove = #g_savedata.tile_ui_ids
	end

	-- 0.1.1.0 changes
	g_savedata.components = --[[g_savedata.components or]] {
		all = {},
		characters = {},
		objects = {},
		zones = {},
		vehicles = {},
		flares = {},
		fires = {},
		loot = {},
		animals = {},
		creatures = {},
		graph_nodes = {}
	}

	g_savedata.location_data = g_savedata.location_data or {}

	g_savedata.addon_data = g_savedata.addon_data or {}

	g_savedata.drawing = g_savedata.drawing or {
		drawn = {},
		drawing = {}, ---@type table<string, table<integer, LineToDraw|LabelToDraw|PolygonToDraw>>
		removing = {},
		next_draw_object_id = 1
	}

	g_savedata.libraries = g_savedata.libraries or {
		ui_id = {
			ui_ids = {
				reserved = {}, ---@type table<SWUI_ID, true> the ui_ids that are in use
				unreserved = {} ---@type table<integer, SWUI_ID> the ui_ids that are not in use and are avaliable
			}
		}
	}

	g_savedata.tick_counter = g_savedata.tick_counter or 0

	-- end of 0.1.1.0 changes

	-- get all of the components

	-- iterate through all addons
	for addon_index = 0, server.getAddonCount() - 1 do
		local addon_data = server.getAddonData(addon_index)

		-- skip addon if theres no locations
		if not addon_data.location_count or addon_data.location_count <= 0 then
			goto continue_addon
		end

		-- iterate through all locations in this addon
		for location_index = 0, addon_data.location_count - 1 do
			local location_data = server.getLocationData(addon_index, location_index)

			-- skip location if its not an environment mod
			if not location_data.env_mod then
				goto continue_location
			end

			-- iterate through all components in this location
			for component_index = 0, location_data.component_count - 1 do
				local component_data, is_success = server.getLocationComponentData(addon_index, location_index, component_index)

				-- ensure the data was gotten
				if not is_success then
					goto continue_component
				end

				-- ensure that this component type is within our table
				if not components[component_data.type] then
					goto continue_component
				end

				-- skip zones (We don't have to guess their locations, we can just use server.getZones())
				if component_data.type == "zone" then
					goto continue_component
				end

				---@type TMComponentData
				local tm_component_data = table.copy.deep(component_data)

				tm_component_data.location_data = location_data
				tm_component_data.addon_data = addon_data

				table.insert(components[tm_component_data.type], tm_component_data)

				::continue_component::
			end

			::continue_location::
		end

		::continue_addon::
	end

	components.zone = server.getZones()
end

function onTick()

	g_savedata.tick_counter = g_savedata.tick_counter + 1

	local actions_remaining = actions_per_tick

	-- if theres things to remove
	if #g_savedata.drawing.removing > 0 then
		-- sort every 6 seconds
		if isTickID(0, 375) then
			g_savedata.drawing.removing = sortByDistanceToPlayers(g_savedata.drawing.removing)
		end

		-- start from top to start from the furthest, also cause we dont want to shift stuff.
		for removing_index = #g_savedata.drawing.removing, 1, -1 do
			UI.deleteID(g_savedata.drawing.removing[removing_index].ui_id)

			-- it was removed, can be removed from the remove queue
			table.remove(g_savedata.drawing.removing, removing_index)

			-- subtract 1 action
			actions_remaining = actions_remaining - 1

			-- if we're out of actions, break
			if actions_remaining <= 0 then
				break
			end
		end
	end

	-- if theres any more actions remaining, add the objects which we need to draw.
	if actions_remaining > 0 then
		-- add the closest <actions_remaining> objects from each list, and then sort it to find the closest ones.
		local closest_to_draw = {} ---@type table<integer, LineToDraw|LabelToDraw|PolygonToDraw>
		for draw_type, objects_to_draw in pairs(g_savedata.drawing.drawing) do
			local added_objects = 0
			for object_to_draw_index = 1, #objects_to_draw do
				-- add this object to the list of objects which are closest to draw
				local object_to_draw = objects_to_draw[object_to_draw_index]

				local combined_object_to_draw = table.copy.deep(object_to_draw) ---@type LineToDraw|LabelToDraw|PolygonToDraw
				-- add the original draw type index, so we can remove it if we've drawn it.
				combined_object_to_draw.original_index = object_to_draw_index

				table.insert(closest_to_draw, combined_object_to_draw)

				added_objects = added_objects + 1

				-- if the added objects hit the actions remaining, then break
				if added_objects >= actions_remaining then
					break
				end
			end
		end

		-- sort by the closest to the players.
		closest_to_draw = sortByDistanceToPlayers(closest_to_draw) ---@type table<integer, LineToDraw|LabelToDraw|PolygonToDraw>

		local closest_by_original_index = {} ---@type table<integer, LineToDraw|LabelToDraw|PolygonToDraw>

		--[[
			sort the closest <actions_remaining> objects by their original index
			to ensure that when we remove them, we're not shifting the next ones we're going to draw
			causing the data to be corrupted.
		]]
		for closest_to_draw_index = 1, math.min(actions_remaining, #closest_to_draw) do
			table.insert(closest_by_original_index, closest_to_draw[closest_to_draw_index])
		end

		table.sort(closest_by_original_index, function(a, b)
			---@diagnostic disable-next-line: undefined-field
			return a.original_index > b.original_index
		end) ---@type table<integer, LineToDraw|LabelToDraw|PolygonToDraw>

		-- draw all of these objects
		for object_index = 1, #closest_by_original_index do
			local object = closest_by_original_index[object_index]

			local ui_id = UI.reserveID() --[[@as SWUI_ID]]

			-- draw as line
			if object.ui_type == "line" then
				server.addMapLine(
					-1,
					ui_id,
					object.start_matrix,
					object.end_matrix,
					object.w,
					object.colour.r,
					object.colour.g,
					object.colour.b,
					object.colour.a
				)
			elseif object.ui_type == "label" then
				server.addMapLabel(
					-1,
					ui_id,
					object.label_type,
					object.name,
					object.transform[13],
					object.transform[15]
				)
			elseif object.ui_type == "polygon" then
				UI.drawPolygon(
					-1,
					ui_id,
					object.transform[13],
					object.transform[15],
					object.width,
					object.height,
					object.sides,
					object.rotation,
					object.line_thickness,
					object.colour
				)
			end

			-- add to the drawn objects table
			table.insert(g_savedata.drawing.drawn[object.draw_type], {
				ui_id = ui_id,
				transform = object.transform
			})

			---@diagnostic disable-next-line: undefined-field
			table.remove(g_savedata.drawing.drawing[object.draw_type], object.original_index)
		end
	end
	--[[if g_savedata.removing_pos and g_savedata.removing_pos > 0 then
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
		--[[end
	end]]
end

-- ensures that this draw type has been created in g_savedata.
function ensureDrawTypeCreated(draw_type)
	g_savedata.drawing.drawing[draw_type] = g_savedata.drawing.drawing[draw_type] or {}
	--g_savedata.drawing.removing[draw_type] = g_savedata.drawing.removing[draw_type] or {}
	g_savedata.drawing.drawn[draw_type] = g_savedata.drawing.drawn[draw_type] or {}
end

function sortByDistanceToPlayers(t, sort_by_furthest)

	local players = server.getPlayers()

	local player_positions = {}

	--[[
		gets the player's position for us
		so we dont have to use server.getPlayerPos for every single object.
	]]
	for player_index = 1, #players do
		local player = players[player_index]
		
		local position, is_success = server.getPlayerPos(player.id)

		if is_success then
			table.insert(player_positions, position)
		end
	end

	--[[
		function to get the closest player distance to an object
		so we dont have to calculate the closest player for every single sort check, instead we've already got it
		so instead we're just comparing numbers.
	]]
	local function getClosestPlayerDistance(transform)
		local closest_distance = math.huge
		for player_pos_index = 1, #player_positions do
			local player_pos = player_positions[player_pos_index]

			local player_distance = math.euclideanDistance(player_pos[13], transform[13], player_pos[15], transform[15])
			if closest_distance > player_distance then
				closest_distance = player_distance
			end
		end

		return closest_distance
	end

	local closest_distances = {}

	-- finds the closest player distance for each object so we don't have to constantly do so for every single sort check.
	for ti = 1, #t do
		t[ti]._pre_sort_index = ti
		closest_distances[ti] = getClosestPlayerDistance(t[ti].transform)
	end

	table.sort(t, function(a, b)
		return math.xor(closest_distances[a._pre_sort_index] < closest_distances[b._pre_sort_index], sort_by_furthest)
	end)

	-- remove _pre_sort_index from all of them.
	for ti = 1, #t do
		t[ti]._pre_sort_index = nil
	end

	return t
end

function queueToDraw(draw_type)

	---@param real_draw_type DrawType the draw type for the object
	---@param x number starting x coordinate of the line
	---@param z number starting z coordinate of the line
	---@param tx number target x coordinate of the line
	---@param tz number target z coordinate of the line
	---@param w number width of the line
	---@param r number red colour, 0-255
	---@param g number green colour, 0-255
	---@param b number blue colour, 0-255
	---@param a number alpha, 0-255
	local function addLineToQueue(real_draw_type, x, z, tx, tz, w, r, g, b, a)
		-- ensure that this draw type is created.
		ensureDrawTypeCreated(real_draw_type)
		
		table.insert(g_savedata.drawing.drawing[real_draw_type],
			{
				ui_type = "line",
				draw_type = real_draw_type,
				start_matrix = matrix.translation(x, 0, z),
				end_matrix = matrix.translation(tx, 0, tz),
				transform = matrix.translation((x+tx)*0.5, 0, (z+tz)*0.5), -- the middle of the line
				w = w,
				colour = {
					r = r,
					g = g,
					b = b,
					a = a
				}
			}
		)
	end

	---@param real_draw_type DrawType the draw type for the object
	---@param x number x coordinate of the label
	---@param z number z coordinate of the label
	---@param label_type SWLabelTypeEnum the label type
	---@param name string the string to display with this label
	local function addLabelToQueue(real_draw_type, x, z, label_type, name)
		-- ensure that this draw type is created.
		ensureDrawTypeCreated(real_draw_type)
		
		table.insert(g_savedata.drawing.drawing[real_draw_type],
			{
				ui_type = "label",
				draw_type = real_draw_type,
				transform = matrix.translation(x, 0, z),
				label_type = label_type,
				name = name
			}
		)
	end

	---@param real_draw_type DrawType the draw type for the object
	---@param x number middle of the polygon on the x axis
	---@param z number middle of the polygon on the z axis
	---@param width number the width the polygon in the x axis (diametre)
	---@param height number the height of the polygon in the z axis (diametre)
	---@param sides integer how many sides to give the polygon (minimum 1, high numbers not recommended for performance purposes)
	---@param rotation number the rotation of the polygon (radians)
	---@param line_thickness number the thickness of the lines on the polygon
	---@param r number red colour, 0-255
	---@param g number green colour, 0-255
	---@param b number blue colour, 0-255
	---@param a number alpha, 0-255
	local function addPolygonToQueue(real_draw_type, x, z, width, height, sides, rotation, line_thickness, r, g, b, a)
		-- ensure that this draw type is created.
		ensureDrawTypeCreated(real_draw_type)

		table.insert(g_savedata.drawing.drawing[real_draw_type],
			{
				ui_type = "polygon",
				draw_type = real_draw_type,
				transform = matrix.translation(x, 0, z),
				width = width,
				height = height,
				sides = sides,
				rotation = rotation,
				line_thickness = line_thickness,
				colour = {
					r = r,
					g = g,
					b = b,
					a = a
				}
			}
		)
	end

	-- remove everything already drawn for this type
	queueToRemove(draw_type)

	-- tile grid drawing
	if ("tile_grid"):match(draw_type) then
		-- draw grid lines

		-- go through each planet.
		for _, planet_map_bounds in pairs(map_bounds) do
		
			-- x axis grid lines
			for x = planet_map_bounds.x.min, planet_map_bounds.x.max, tile_length do
				addLineToQueue("tile_grid", x, planet_map_bounds.z.min, x, planet_map_bounds.z.max, 0.75, 85, 85, 85, 255)
			end

			-- z axis grid lines
			for z = planet_map_bounds.z.min, planet_map_bounds.z.max, tile_length do
				addLineToQueue("tile_grid", planet_map_bounds.x.min,z, planet_map_bounds.x.max, z, 0.75, 85, 85, 85, 255)
			end
		end

		g_savedata.drawing.drawing.tile_grid = sortByDistanceToPlayers(g_savedata.drawing.drawing.tile_grid)
	end

	-- tile drawing
	if ("tile"):match(draw_type) then

		-- queue the tiles to be drawn
		-- go through each planet.
		for _, planet_map_bounds in pairs(map_bounds) do
			-- x axis
			for x = planet_map_bounds.x.min, planet_map_bounds.x.max - tile_length + 1, tile_length do
				-- z axis
				for z = planet_map_bounds.z.min, planet_map_bounds.z.max - tile_length + 1, tile_length do
					local tile_transform = matrix.translation(x, 0, z)
					local tile_data = server.getTile(tile_transform)
					if tile_data then
						-- put in top left corner
						
						if tile_data.name == nil or tile_data.name == "" then
							tile_data.name = "ocean"
						else
							tile_data.name = tile_data.name:gsub("(.*)/(.*)%.xml", "%2")
						end
						
						addLabelToQueue("tile", x + 50, z + 950, 1, tile_data.name)
					end
				end
			end
		end

		g_savedata.drawing.drawing.tile = sortByDistanceToPlayers(g_savedata.drawing.drawing.tile)
	end

	-- zone drawing
	if ("env_zone"):match(draw_type) then
		-- queue the zones to be drawn
		for zone_index = 1, #components.zone do
			local zone_data = components.zone[zone_index]

			local roll, yaw, pitch = matrix.getMatrixRotation(zone_data.transform)

			addPolygonToQueue(
				"env_zone",
				zone_data.transform[13],
				zone_data.transform[15],
				zone_data.size.x,
				zone_data.size.z,
				4,
				yaw,
				0.65,
				200,
				20,
				20,
				255
			)
		end

		g_savedata.drawing.drawing.env_zone = sortByDistanceToPlayers(g_savedata.drawing.drawing.env_zone)
	end

	-- graph_node drawing
	if ("env_graph_node"):match(draw_type) then
		-- queue the graph nodes to be drawn
		--[[for graph_node_index = 1, #components.graph_node do
			local graph_node_data = components.graph_node[graph_node_index]

			local transform = matrix.multiply(graph_node_data.location_data.transform)

			addPolygonToQueue(
				"env_graph_node",
				transform[13],
				transform[15],
				5,
				5,
				4,
				0,
				0.65,
				20,
				255,
				255,
				255
			)
		end

		g_savedata.drawing.drawing.env_graph_node = sortByDistanceToPlayers(g_savedata.drawing.drawing.env_graph_node)]]
	end
end

function queueToRemove(draw_type)

	local function removeFromDrawn(real_draw_type)
		ensureDrawTypeCreated(real_draw_type)

		--INFO("Adding all "..real_draw_type.." To remove list...")
		for drawn_index = 1, #g_savedata.drawing.drawn[real_draw_type] do
			--INFO("Adding "..drawn_index.." To remove list")
			local drawn_object = g_savedata.drawing.drawn[real_draw_type][drawn_index]
			table.insert(g_savedata.drawing.removing, {
				ui_id = drawn_object.ui_id,
				transform = drawn_object.transform
			})
			--INFO("#g_savedata.drawing.removing: "..tostring(#g_savedata.drawing.removing))
		end

		g_savedata.drawing.drawn[real_draw_type] = {}
	end

	if ("tile_grid"):match(draw_type) then
		-- clear draw queue
		g_savedata.drawing.drawing.tile_grid = nil

		removeFromDrawn("tile_grid")
 	end

	if ("tile"):match(draw_type) then
		-- clear draw queue
		g_savedata.drawing.drawing.tile = nil

		removeFromDrawn("tile")
	end

	if ("env_zone"):match(draw_type) then
		-- clear draw queue
		g_savedata.drawing.drawing.env_zone = nil

		removeFromDrawn("env_zone")
	end

	if ("env_graph_node"):match(draw_type) then
		-- clear draw queue
		g_savedata.drawing.drawing.env_graph_node = nil

		removeFromDrawn("env_graph_node")
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

	local arg = table.pack(...)

	--*---
	--* handle the command the player entered
	--*---
	
	if command == "draw" then

		local draw_type = arg[1]

		if not draw_type then
			draw_type = "tile"
		end

		draw_type:lower()

		local draw_aliases = {
			all = ".*",
		}

		if draw_aliases[draw_type] then
			draw_type = draw_aliases[draw_type]
		end

		queueToDraw(draw_type)

		-- sort the objects to remove.
		g_savedata.drawing.removing = sortByDistanceToPlayers(g_savedata.drawing.removing)

		--[[if not g_savedata.ui_id then
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
		
		g_savedata.drawing_pos = #g_savedata.tiles_to_draw]]
		
		
	elseif command == "remove" then

		local draw_type = arg[1]

		if not draw_type then
			draw_type = ".*"
		end

		draw_type:lower()

		local draw_aliases = {
			all = ".*",
		}

		if draw_aliases[draw_type] then
			draw_type = draw_aliases[draw_type]
		end

		queueToRemove(draw_type)

		-- sort the objects to remove.
		g_savedata.drawing.removing = sortByDistanceToPlayers(g_savedata.drawing.removing)

		--[[if g_savedata.ui_id then
			server.removeMapID(-1, g_savedata.ui_id)
		end
		
		g_savedata.removing_pos = 1]]
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

---@param id integer the tick you want to check that it is
---@param rate integer the total amount of ticks, for example, a rate of 60 means it returns true once every second* (if the tps is not low)
---@return boolean isTick if its the current tick that you requested
function isTickID(id, rate)
	return (g_savedata.tick_counter + id) % rate == 0
end