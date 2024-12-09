-- Author: Michael Adler
local M = {}

--- @class Block
--- @field id nil | number
--- @field size number
local Block = {}
Block.__index = Block

--- @return Block
function Block.new(_, id, size)
    local instance = setmetatable({}, Block)
    instance.id = id
    instance.size = size
    return instance
end

function Block:is_file()
    return self.id ~= nil
end

function Block:__tostring()
    if self.id == nil then -- empty block
        return string.rep(".", self.size)
    end
    return string.rep(tostring(self.id), self.size)
end

--- @return Block[]
local function parse(input)
    local blocks = {}

    local file_id = 0
    local disk_map = input:match("%d+")
    for i = 1, #disk_map do
        local length = tonumber(disk_map:sub(i, i))
        if i % 2 ~= 0 then
            local blk = Block:new(file_id, length)
            table.insert(blocks, blk)
            file_id = file_id + 1
        else
            if length > 0 then -- skip empty blocks
                local blk = Block:new(nil, length)
                table.insert(blocks, blk)
            end
        end
    end
    return blocks
end

--- @param blocks Block[]
local function checksum(blocks)
    local result = 0
    local i = 0
    for _, blk in ipairs(blocks) do
        if blk:is_file() then
            for _ = 1, blk.size do
                result = result + blk.id * i
                i = i + 1
            end
        else
            i = i + blk.size
        end
    end
    return result
end

local function seek_empty(blocks, start)
    local i = start
    local n = #blocks
    while i <= n do
        if blocks[i]:is_file() == false and blocks[i].size > 0 then
            return i
        end
        i = i + 1
    end
    return nil
end

local function seek_file(blocks, start)
    local i = start
    while i >= 1 do
        if blocks[i]:is_file() == true and blocks[i].size > 0 then
            return i
        end
        i = i - 1
    end
    return nil
end

--- @param blocks Block[]
local function defrag(blocks)
    -- two indices: empty moving forwards, file moving backwards
    local idx_empty = seek_empty(blocks, 1)
    local idx_file = seek_file(blocks, #blocks)
    while idx_empty ~= nil and idx_file ~= nil and idx_empty < idx_file do
        -- move data from file block to empty block (but not more than empty block can hold!)
        local empty_size = blocks[idx_empty].size
        local file_size = blocks[idx_file].size
        local size = math.min(file_size, empty_size)
        blocks[idx_empty].id = blocks[idx_file].id -- id is preserved
        blocks[idx_empty].size = size
        blocks[idx_file].size = file_size - size
        -- insert new empty block after our moved file block
        local delta = empty_size - size
        if delta > 0 then
            table.insert(blocks, idx_empty + 1, Block:new(nil, delta))
        end
        idx_empty = seek_empty(blocks, idx_empty)
        idx_file = seek_file(blocks, idx_file)
    end
end

--- @param blocks Block[]
local function defrag_2(blocks)
    local file_ids = {}
    for _, blk in pairs(blocks) do
        if blk:is_file() then
            table.insert(file_ids, blk.id)
        end
    end
    table.sort(file_ids, function(a, b)
        return a > b
    end)

    while file_ids[1] ~= nil do -- while not empty
        local file_id = table.remove(file_ids, 1)
        local idx_file
        for i = #blocks, 1, -1 do
            if blocks[i].id == file_id then
                idx_file = i
                break
            end
        end

        local file_size = blocks[idx_file].size
        -- find empty block which can hold our file
        local idx_empty
        for i = 1, idx_file - 1 do -- only move to the left
            if blocks[i]:is_file() == false and blocks[i].size >= file_size then
                idx_empty = i
                break
            end
        end
        if idx_empty then
            local delta = blocks[idx_empty].size - file_size

            -- move whole file
            blocks[idx_empty].id = blocks[idx_file].id -- id is preserved
            blocks[idx_empty].size = file_size

            -- mark as free space
            blocks[idx_file].id = nil
            -- insert new empty block after our moved file block
            if delta > 0 then
                table.insert(blocks, idx_empty + 1, Block:new(nil, delta))
            end
        end
    end
end

--- @param input string
M.solve = function(input)
    local blocks = parse(input)
    local blocks_2 = parse(input)

    defrag(blocks)
    defrag_2(blocks_2)

    return checksum(blocks), checksum(blocks_2)
end

return M
