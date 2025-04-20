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

---@diagnostic disable:duplicate-doc-field
---@diagnostic disable:duplicate-doc-alias
---@diagnostic disable:duplicate-set-field

--[[
	This library is used to have a more optimised system of using ui_ids
	Whenever a ui_id is no longer used by this script, it puts it as unreserved, and will use it
	for the next UI element that requires a new ui_id.
	and will automatically generate new ones when theres no unreserved ones left.

	Extension and Dependency of the UI library.
]]
--[[
---@class UI_TypeHistory the history for this ui type
---@field is_drawn boolean if this type is being used at all with the ui_id its linked to
---@field peers table<integer, integer> the players that it is drawn for.

---@class UI_ID_History
---@field object UI_TypeHistory the history for objects with this ui_id
---@field label UI_TypeHistory the history for labels with this ui_id
---@field line UI_TypeHistory the history for lines with this ui_id
---@field popup UI_TypeHistory the history for popups and popup screens with this ui_id
]]

g_savedata.libraries.ui_id = {
	ui_ids = {
		reserved = {}, ---@type table<SWUI_ID, true> the ui_ids that are in use
		unreserved = {} ---@type table<integer, SWUI_ID> the ui_ids that are not in use and are avaliable
	}
}

g_ui_id = g_savedata.libraries.ui_id

-- library name
UI = UI or {}

---# Reserves a UI ID, if theres any unreserved ids avaliable, it will use that, otherwise it will generate a new ui_id
---@return SWUI_ID ui_id the now reserved ui_id
function UI.reserveID()
	local unreserved_ui_ids = #g_ui_id.ui_ids.unreserved
	if unreserved_ui_ids == 0 then
		-- generate a new ui_id
		local new_ui_id = server.getMapID() --[[@as SWUI_ID]]

		g_ui_id.ui_ids.reserved[new_ui_id] = true

		return new_ui_id
	end

	local unreserved_ui_id = g_ui_id.ui_ids.unreserved[unreserved_ui_ids]

	table.remove(g_ui_id.ui_ids.unreserved, unreserved_ui_ids)

	g_ui_id.ui_ids.reserved[unreserved_ui_id] = true

	return unreserved_ui_id
end

---# Deletes a UI_ID and sets it as unreserved
---@param ui_id SWUI_ID the ui_id to remove
function UI.deleteID(ui_id)

	-- add it to the list of unreserved ui_ids
	table.insert(g_ui_id.ui_ids.unreserved, ui_id)

	-- remove it from the map
	server.removeMapID(-1, ui_id)
end