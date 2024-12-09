-- Author: Michael Adler
local M = {}

--- @class Block
--- @field id nil | number
--- @field size number
--- @field has_moved boolean
local Block = {}
Block.__index = Block

--- @return Block
function Block.new(_, id, size)
    local instance = setmetatable({}, Block)
    instance.id = id
    instance.size = size
    instance.has_moved = false -- for part 2
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

local function print_disk(blocks)
    for _, blk in pairs(blocks) do
        io.stdout:write(tostring(blk))
    end
    print()
end

--- @param blocks Block[]
local function defrag_2(blocks)
    local idx_file = seek_file(blocks, #blocks)
    local idx_empty = 1
    while idx_file >= 1 do
        -- print_disk(blocks)
        -- print(">> looking for a new home for file id", blocks[idx_file].id)

        local file_size = blocks[idx_file].size

        for i = 1, idx_file - 1 do
            if blocks[i]:is_file() == false and blocks[i].size >= file_size then
                idx_empty = i
                break
            end
        end
        if idx_empty then
            local empty_size = blocks[idx_empty].size
            if empty_size >= file_size then -- only move whole file
                -- print(">> YES", idx_empty)
                local size = file_size
                blocks[idx_empty].id = blocks[idx_file].id -- id is preserved
                blocks[idx_empty].size = size
                blocks[idx_empty].has_moved = true -- remember that we have moved this block

                blocks[idx_file].id = nil
                -- insert new empty block after our moved file block
                local delta = empty_size - size
                if delta > 0 then
                    table.insert(blocks, idx_empty + 1, Block:new(nil, delta))
                end
            end
        else
            -- print(">> NO")
        end

        repeat
            idx_file = idx_file - 1
        until idx_file < 1
            or (
                blocks[idx_file]:is_file() == true
                and blocks[idx_file].size > 0
                and blocks[idx_file].has_moved == false
            )
    end
    -- print_disk(blocks)
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
