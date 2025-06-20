require("libraries.utils.math")

---@param matrix1 SWMatrix the first matrix
---@param matrix2 SWMatrix the second matrix
---@return number distance the xz distance between the two matrices
function matrix.xzDistance(matrix1, matrix2) -- returns the euclidean distance between two matrixes, ignoring the y axis
	return math.euclideanDistance(matrix1[13], matrix2[13], matrix1[15], matrix2[15])
end

---@param rot_matrix SWMatrix the matrix you want to get the rotation of
---@return number x_axis the x_axis rotation (roll)
---@return number y_axis the y_axis rotation (yaw)
---@return number z_axis the z_axis rotation (pitch)
function matrix.getMatrixRotation(rot_matrix) --returns radians for the functions: matrix.rotation X and Y and Z (credit to woe and quale)
	local z = -math.atan(rot_matrix[5],rot_matrix[1])
	rot_matrix = matrix.multiply(rot_matrix, matrix.rotationZ(-z))
	return math.atan(rot_matrix[7],rot_matrix[6]), math.atan(rot_matrix[9],rot_matrix[11]), z
end

---@param matrix1 SWMatrix the first matrix
---@param matrix2 SWMatrix the second matrix
---@return SWMatrix matrix the multiplied matrix
function matrix.multiplyXZ(matrix1, matrix2)
	local matrix3 = {table.unpack(matrix1)}
	matrix3[13] = matrix3[13] + matrix2[13]
	matrix3[15] = matrix3[15] + matrix2[15]
	return matrix3
end

--# returns the total velocity (m/s) between the two matrices
---@param matrix1 SWMatrix the first matrix
---@param matrix2 SWMatrix the second matrix
---@param ticks_between number the ticks between the two matrices
---@return number velocity the total velocity
function matrix.velocity(matrix1, matrix2, ticks_between)
	ticks_between = ticks_between or 1
	-- total velocity
	return math.euclideanDistance(matrix1[13], matrix2[13], matrix1[15], matrix2[15], matrix1[14], matrix2[14]) * 60/ticks_between
end

--# returns the acceleration, given 3 matrices. Each matrix must be the same ticks between eachother.
---@param matrix1 SWMatrix the most recent matrix
---@param matrix2 SWMatrix the second most recent matrix
---@param matrix3 SWMatrix the third most recent matrix
---@return number acceleration the acceleration in m/s
function matrix.acceleration(matrix1, matrix2, matrix3, ticks_between)
	local v1 = matrix.velocity(matrix1, matrix2, ticks_between) -- last change in velocity
	local v2 = matrix.velocity(matrix2, matrix3, ticks_between) -- change in velocity from ticks_between ago
	-- returns the acceleration
	return (v1-v2)/(ticks_between/60)
end