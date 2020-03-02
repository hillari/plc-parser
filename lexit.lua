-- Hillari M. Denny
-- CSCE A331 Assignment 3

-- =====================================================================
-- Exercise 1 - Running a Forth Program
-- =====================================================================

-- On my OS (Linux Mint) I ran:
-- >> apt-get gforth
-- >> gforth

-- Entering "include check_forth.fs" yielded the following output:

-- Secret message #3:

-- I am one with the Forth. The Forth is with me.

-- =====================================================================
-- Exercise 2 - Lexer in Lua
-- lexit.lua
-- @ Glen G. Chappell, Hillari M. Denny


-- This code is based on the lexer example provided by Dr. Chappell.

-- This program is a lexical analyzer for the Programming Language "Degu".
-- It uses some of the structure and functions provided by Dr. Chappel
-- In the code procided in lecture and on github. Each function contains 
-- the author names and a brief description of it's purpose.
--
-- It passes all tests in lexit_test.lua 
-- Overall, this was a very enjoyable programming assignment and the 
-- test suites provided by Dr. Chappell are amazing!!
-- =====================================================================

-- *********************************************************************
-- Module Table Initialization
-- *********************************************************************


local lexit = {}  -- Our module; members are added below


-- *********************************************************************
-- Public Constants
-- *********************************************************************

-- Numeric constants representing lexeme categories
lexit.KEY      = 1
lexit.ID       = 2
lexit.NUMLIT   = 3
lexit.STRLIT   = 4
lexit.OP 	   = 5
lexit.PUNCT    = 6
lexit.MAL 	   = 7

-- catnames
-- Array of names of lexeme categories.
-- Human-readable strings. Indices are above numeric constants.
lexit.catnames = {
    "Keyword",
    "Identifier",
    "NumericLiteral",
    "StringLiteral",
    "Operator",
    "Punctuation",
    "Malformed"
}


-- *********************************************************************
-- Kind-of-Character Functions
-- *********************************************************************

-- All functions return false when given a string whose length is not
-- exactly 1.

-- isLetter
-- @ Glenn Chappell
-- Returns true if string c is a letter character, false otherwise.
local function isLetter(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "A" and c <= "Z" then
        return true
    elseif c >= "a" and c <= "z" then
        return true
    else
        return false
    end
end


-- isDigit
-- @ Glenn Chappell
-- Returns true if string c is a digit character, false otherwise.
local function isDigit(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "0" and c <= "9" then
        return true
    else
        return false
    end
end

-- isWhitespace
-- @ Glenn Chappell and Hillari Denny

-- Returns true if string c is a whitespace character, false otherwise.
-- According the lexeme specifications, whitespace characters include;
-- blank, tab(t), vertical-tab(v), new-line(n), carriage-return(r), form-feed(f)
local function isWhitespace(c)
    if c:len() ~= 1 then
        return false
    elseif c == " " or c == "\t" or c == "\n" or c == "\r"
      or c == "\f" or c == "\v" then
        return true
    else
        return false
    end
end

-- isPrintableASCII
-- @ Glenn Chappell
-- Returns true if string c is a printable ASCII character (codes 32 " "
-- through 126 "~"), false otherwise.
local function isPrintableASCII(c)
    if c:len() ~= 1 then
        return false
    elseif c >= " " and c <= "~" then
        return true
    else
        return false
    end
end

-- isIllegal
-- @ Glenn Chappell
-- Returns true if string c is an illegal character, false otherwise.
local function isIllegal(c)
    if c:len() ~= 1 then
        return false
    elseif isWhitespace(c) then
        return false
    elseif isPrintableASCII(c) then
        return false
    else
        return true
    end
end


-- *********************************************************************
-- The lexit
-- *********************************************************************

-- lex
-- @ Glenn Chappell and Hillari Denny
-- Our lexit
-- Intended for use in a for-in loop:
--     for lexstr, cat in lexit.lex(program) do
-- Here, lexstr is the string form of a lexeme, and cat is a number
-- representing a lexeme category. (See Public Constants.)
function lexit.lex(program)
    -- ***** Variables (like class data members) *****

    local pos       -- Index of next character in program
                    -- INVARIANT: when getLexeme is called, pos is
                    --  EITHER the index of the first character of the
                    --  next lexeme OR program:len()+1
    local state     -- Current state for our state machine
    local ch        -- Current character
    local lexstr    -- The lexeme, so far
    local category  -- Category of lexeme, set when state set to DONE
    local handlers  -- Dispatch table; value created later

    -- ***** States *****

    local DONE       = 0
    local START      = 1
    local LETTER     = 2
    local DIGIT      = 3
    local EXP        = 4
    local STRLIT     = 5


    -- ***** Character-Related Utility Functions *****

    -- currChar
    -- @ Glenn Chappell
    -- Return the current character, at index pos in program. Return
    -- value is a single-character string, or the empty string if pos is
    -- past the end.
    local function currChar()
        return program:sub(pos, pos)
    end

    -- drop1
    -- @ Glenn Chappell
    -- Move pos to the next character.
    local function drop1()
        pos = pos+1
    end

    -- add1
    -- @ Glenn Chappell
    -- Add the current character to the lexeme, moving pos to the next
    -- character.
    local function add1()
        lexstr = lexstr .. currChar()
        drop1()
    end

    -- skipWhitespace
    -- @ Glenn Chappell
    --
    -- Skip whitespace and comments, moving pos to the beginning of
    -- the next lexeme, or to program:len()+1.

    -- Only modification is the check for comment character, and checking for newline
    local function skipWhitespace()
        while true do      -- In whitespace
            while isWhitespace(currChar()) do
                drop1()
            end

            if currChar() ~= "#" then  -- comment
                break
            end
            drop1()

            while true do  -- In comment
                if currChar() == "" or currChar() == "\n" then -- end of input or newline
                    drop1()
                    break
                end
                drop1()
            end
        end
    end

    -- lookAhead
    -- @ Hillari Denny
    --
	-- takes a number and returns that number of chars ahead of current position
    -- this was created for when we want to look ahead more than 1 char, and 
    -- replaces currChar() from Dr. Chappells code
    local function lookAhead(n)
        return program:sub(pos+n, pos+n)
    end


    -- ***** State-Handler Functions *****

    -- A function with a name like handle_XYZ is the handler function
    -- for state XYZ

    -- @ Glenn Chappell
    local function handle_DONE()
        error("'DONE' state should not be handled\n")
    end

    -- handle_START 
    -- @ Glenn Chappell and Hillari Denny

    -- function to handle checking which state we need to transition to on first character.
    -- Handling of operators is done here. This could probably be moved to a separate funciton 
    -- for brevity but...¯\_(ツ)_/¯
    local function handle_START()
    	if isIllegal(ch) then
    		add1()
    		state = DONE
    		category = lexit.MAL
    	elseif isLetter(ch) or ch == "_" then
    		add1()
    		state = LETTER
    	elseif isDigit(ch) then
    		add1()
    		state = DIGIT
    	elseif ch == "'" or ch == '"' then 
            if ch == "'" then
                endquote = "'" 
                add1()
                state = STRLIT
            else
                endquote = '"'
                add1()
                state = STRLIT 
            end
        elseif 
            -- Handle the double operators first and add both if they are part of specification
            ch == "<" and lookAhead(1) == "=" or
            ch == ">" and lookAhead(1) == "=" or
            ch == "=" and lookAhead(1) == "=" or
            ch == "!" and lookAhead(1) == "=" then
                add1()
                add1()
                state = DONE
                category = lexit.OP
        elseif 
            ch == "<" or ch == ">" or ch == "+" or
            ch == "-" or ch == "*" or ch == "/" or
            ch == "%" or ch == "[" or ch == "]"  or ch == "=" then
                add1()
                state = DONE
                category = lexit.OP
    	else 
    		add1()
    		state = DONE
    		category = lexit.PUNCT
    	end
    end

    -- ** handle_STRLIT
    -- @ Hillari Denny

    -- This function first checks if we have an escape character. If we do, 
    -- we check for a malformed string by using lookahead. We then check 
    -- for another malformed string or the correct ending quote (using the
    -- global variable we set in handle_START). Otherwise we're still in
    -- a valid string literal and can keep adding chars. 
    local function handle_STRLIT()
        if ch == '\\' then
            checkend = lookAhead(2)
            if checkend == '' then -- if we have escape and reach end of input before ending quote
                add1()
                add1()
                state = DONE
                category = lexit.MAL
            else -- otherwise we can add the escape char and the escaped char
                add1()
                add1()
            end

        elseif ch == '' then -- If we reach end of input before end quote
            state = DONE
            category = lexit.MAL
        elseif ch == endquote then
            add1()
            state = DONE
            category = lexit.STRLIT
        else
            add1() -- keep adding while we have valid string literal
        end
    end

    -- handle_DIGIT
    -- @ Hillari Denny 

    -- Based loosley on the functions provided by Dr. Chappel in lecture 
    -- and github. First we check if we have a continuing numeric literal.
    -- We then use check for e/E and use lookahead to see if it's an expopnent.
    -- If so, we go to the EXP state. Otherwise, we are done with our numlit.
    -- Lastly, if it's no longer a digit or an exponent, we are also done.
    local function handle_DIGIT()
    	if isDigit(ch) then
    		add1()
    	elseif ch == "e" or ch == "E" then -- legal exponent, now we need to check next two characters
            nextCh = lookAhead(1) 
            nextnextCh = lookAhead(2) 
    		if isDigit(nextCh) then --legal exponent followed by digit
    			add1()
    			state = EXP  
            elseif nextCh == "+" and isDigit(nextnextCh) then -- exponent followed by a "+" and digit
                add1()
                add1()
                state = EXP
    		else -- If we have something else after the e, we know it's not an exponent
    			state = DONE 
    			category = lexit.NUMLIT
    		end
    	else
    		state = DONE
    		category = lexit.NUMLIT
    	end
    end

    -- handle_EXP
    -- @ Hillari Denny 

    -- Function to handle whether or not we have a continuing digit after our legal exponent
    local function handle_EXP()
        if isDigit(ch) then
            add1()
        else
            state = DONE
            category = lexit.NUMLIT
        end
    end

   -- handle_LETTER
   -- @ Glenn Chappell and Hillari Denny 

   -- Function determines if we have a keyword or ID
   local function handle_LETTER()
   		if isLetter(ch) or isDigit(ch) or ch == "_" then -- we have a letter followed by another letter or digit
   			add1()
   		else
   			state = DONE
   			if lexstr == "and" or lexstr == "char" or lexstr == "elif" or lexstr == "else" or
   				 lexstr == "end" or lexstr == "false" or lexstr == "func" or lexstr == "if" or
   				 lexstr == "input" or lexstr == "not" or lexstr == "or" or lexstr == "print" or
   				 lexstr == "return" or lexstr == "true" or lexstr == "while" then
   				 category = lexit.KEY
   			else
   				category = lexit.ID
   			end
   		end
   end


       -- ***** Table of State-Handler Functions *****

	    handlers = {
	        [DONE]=handle_DONE,
	        [START]=handle_START,
	        [LETTER]=handle_LETTER,
            [DIGIT]=handle_DIGIT,
	        [EXP]=handle_EXP,
            [STRLIT]=handle_STRLIT,
	    }

    -- -- ***** Iterator Function *****

    -- getLexeme
    -- @ Glenn Chappell
    -- Called each time through the for-in loop.
    -- Returns a pair: lexeme-string (string) and category (int), or
    -- nil, nil if no more lexemes.
    local function getLexeme(dummy1, dummy2)
        if pos > program:len() then
            return nil, nil
        end
        lexstr = ""
        state = START
        while state ~= DONE do
            ch = currChar()
            handlers[state]()
        end

        skipWhitespace()
        return lexstr, category
    end

    -- ***** Body of Function lex *****

    -- Initialize & return the iterator function
    pos = 1
    skipWhitespace()
    return getLexeme, nil, nil
end


-- *********************************************************************
-- Module Table Return
-- *********************************************************************


return lexit