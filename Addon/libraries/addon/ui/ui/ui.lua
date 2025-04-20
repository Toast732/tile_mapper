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

require("libraries.utils.math")
require("libraries.addon.ui.ui_id.ui_id")

---@diagnostic disable:duplicate-doc-field
---@diagnostic disable:duplicate-doc-alias
---@diagnostic disable:duplicate-set-field

--[[
	This library adds some new UI functions useful for drawing more complex things,
	such as polygons on the map.
]]

--g_savedata.libraries.ui = {}

--g_ui = g_savedata.libraries.ui

-- library name
UI = UI or {}

---@param peer_id integer the peer_ids to draw it for, use -1 for all.
---@param ui_id SWUI_ID the ui_id to use for drawing the polygon.
---@param x number the middle of the polygon in the x axis
---@param z number the middle of the polygon in the z axis
---@param width number the width the polygon in the x axis (diametre)
---@param height number the height of the polygon in the z axis (diametre)
---@param sides integer how many sides to give the polygon (minimum 1, high numbers not recommended for performance purposes)
---@param rotation number the rotation of the polygon (radians)
---@param line_thickness number the thickness of the lines on the polygon
---@param colour Colour the colour of the lines
function UI.drawPolygon(peer_id, ui_id, x, z, width, height, sides, rotation, line_thickness, colour)
	local angle_increment = math.pi*2/sides
	local angle_start = (math.pi * 2 - angle_increment) / 2

	-- idk, it wasn't fixing, so just add half a radian.
	rotation = rotation + math.pi*0.5

	local vertices = {}

	local width_half = width*0.5
	local height_half = height*0.5

	-- get the vertices
	for side = 0, sides - 1 do

		local angle = angle_start + angle_increment * side

		local scaled_x = width_half * math.cos(angle)
		local scaled_z = height_half * math.sin(angle)

		local rotated_x = scaled_x * math.sin(rotation) + scaled_z * math.cos(rotation)
		local rotated_z = scaled_x * math.cos(rotation) - scaled_z * math.sin(rotation)

		table.insert(vertices, matrix.translation(
			x + rotated_x,
			0,
			z + rotated_z
		))
	end

	-- draw the polygon
	local last_vertex = vertices[#vertices]
	for vertex_index = 1, #vertices do
		local vertex = vertices[vertex_index]

		server.addMapLine(peer_id, ui_id, last_vertex, vertex, line_thickness, colour.r, colour.g, colour.b, colour.a)

		last_vertex = vertex
	end
end

---# Alias for server.removeMapLine, exists for consistency purposes.
---@param peer_id integer the peer_ids to remove the polygon for
---@param ui_id SWUI_ID the ui_id of the polygon.
function UI.removePolygon(peer_id, ui_id)
	server.removeMapLine(-1, ui_id)
end