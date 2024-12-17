-- Author: Michael Adler
local M = {}

local bit = require("bit")
local bxor = bit.bxor
local pow = math.pow

--- @param input string
M.solve = function(input)
    local prog = {}
    local prog_count = 0
    local reg_A, reg_B, reg_C

    local parse = function()
        local i = 1
        for line in input:gmatch("[^\r\n]+") do
            if i == 1 then
                local num = line:match("Register A: (%d+)")
                reg_A = tonumber(num)
            elseif i == 2 then
                local num = line:match("Register B: (%d+)")
                reg_B = tonumber(num)
            elseif i == 3 then
                local num = line:match("Register C: (%d+)")
                reg_C = tonumber(num)
            else
                for num in line:gmatch("(%d+)") do
                    prog[prog_count] = tonumber(num)
                    prog_count = prog_count + 1
                end
            end
            i = i + 1
        end
    end
    parse()

    local function combo_operand(operand)
        --  Combo operands 0 through 3 represent literal values 0 through 3.
        if operand >= 0 and operand <= 3 then
            return operand
        end
        --  Combo operand 4 represents the value of register A.
        if operand == 4 then
            return reg_A
        end
        --  Combo operand 5 represents the value of register B.
        if operand == 5 then
            return reg_B
        end
        --  Combo operand 6 represents the value of register C.
        if operand == 6 then
            return reg_C
        end
        --  Combo operand 7 is reserved and will not appear in valid programs.
        error(string.format("invalid operand: %d", operand))
    end

    -- run program
    local ip = 0
    local output = {}
    while ip < prog_count do
        local opcode, operand = prog[ip], prog[ip + 1]
        if opcode == 0 then
            -- The adv instruction (opcode 0) performs division. The numerator
            -- is the value in the A register. The denominator is found by
            -- raising 2 to the power of the instruction's combo operand.
            -- The result of the division operation is truncated to an integer
            -- and then written to the A register.
            local numerator, denominator = reg_A, pow(2, combo_operand(operand))
            reg_A = math.floor(numerator / denominator)
        elseif opcode == 1 then
            -- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and
            -- the instruction's literal operand, then stores the result in register B.
            reg_B = bxor(reg_B, operand)
        elseif opcode == 2 then
            -- The bst instruction (opcode 2) calculates the value of its combo
            -- operand modulo 8 (thereby keeping only its lowest 3 bits), then
            -- writes that value to the B register.
            reg_B = combo_operand(operand) % 8
        elseif opcode == 3 then
            -- The jnz instruction (opcode 3) does nothing if the A register is
            -- 0. However, if the A register is not zero, it jumps by setting
            -- the instruction pointer to the value of its literal operand; if
            -- this instruction jumps, the instruction pointer is not increased
            -- by 2 after this instruction.
            if reg_A ~= 0 then
                ip = operand
                goto continue
            end
        elseif opcode == 4 then
            -- The bxc instruction (opcode 4) calculates the bitwise XOR of
            -- register B and register C, then stores the result in register B.
            -- (For legacy reasons, this instruction reads an operand but
            -- ignores it.)
            reg_B = bxor(reg_B, reg_C)
        elseif opcode == 5 then
            -- The out instruction (opcode 5) calculates the value of its combo
            -- operand modulo 8, then outputs that value. (If a program outputs
            -- multiple values, they are separated by commas.)
            local out = combo_operand(operand) % 8
            table.insert(output, out)
        elseif opcode == 6 then
            -- The bdv instruction (opcode 6) works exactly like the adv
            -- instruction except that the result is stored in the B register.
            -- (The numerator is still read from the A register.)
            local numerator, denominator = reg_A, pow(2, combo_operand(operand))
            reg_B = math.floor(numerator / denominator)
        elseif opcode == 7 then
            -- The cdv instruction (opcode 7) works exactly like the adv
            -- instruction except that the result is stored in the C register.
            -- (The numerator is still read from the A register.)
            local numerator, denominator = reg_A, pow(2, combo_operand(operand))
            reg_C = math.floor(numerator / denominator)
        end

        ip = ip + 2
        ::continue::
    end
    local part1 = table.concat(output, ",")

    return part1, 0
end

return M
