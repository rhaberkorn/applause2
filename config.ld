-- LDoc configuration file

project = "Applause"
description = "LuaJIT-based real-time synthesizer, based on a stream algebra"

format = "discount"
readme = "README.md"
examples = "examples/"

file = {
	"applause.lua", "sndfile-stream.lua", -- "sndfile.lua",
	"filters.lua", "dssi.lua", "midi.lua", "evdev.lua"
}

no_space_before_args = true
manual_url "https://www.lua.org/manual/5.1/manual.html"

-- Support external references to the bit module.
custom_see_handler("^bit%.(.+)$", function(fnc)
	return "bit."..fnc, "https://bitop.luajit.org/api.html#"..fnc
end)

-- Use @Stream only if a parameter must already be a Stream object.
tparam_alias("Stream", "Stream")
-- Use @StreamableNumber for parameters that can be converted to number streams using tostream().
tparam_alias("StreamableNumber", "Stream|{number,...}|number")

-- For metamethods
-- FIXME: Perhaps we can put them into separate sections.
new_type("metamethod", "Metamethods")

-- The @submodule tag does not work, so we use `@module applause` multiple times instead.
merge = true
