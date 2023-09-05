local sndfile = require "sndfile"

SndfileStream = DeriveClass(Stream)

function SndfileStream:ctor(filename, samplerate, channels, format)
	-- FIXME: This fails if the file is not at the
	-- correct sample rate. Need to resample...
	-- NOTE: samplerate and channels are ignored unless SF_FORMAT_RAW
	-- files are read.
	local handle = sndfile:new(filename, "SFM_READ",
	                           samplerate, channels, format)
	self.filename = filename
	self.samplerate = handle.info.samplerate
	self.channel_no = handle.info.channels
	self.format = handle.info.format
	self.frames = tonumber(handle.info.frames)
	handle:close()

	if self.channel_no > 1 then
		local cached = self:cache()
		local streams = {}
		for i = 0, self.channel_no-1 do
			streams[i+1] = cached:map(function(frame)
				return tonumber(frame[i])
			end)
		end
		return MuxStream:new(unpack(streams))
	end
end

function SndfileStream:gtick()
	-- The file is reopened, so each tick has an independent
	-- read pointer which is important when reusing the stream.
	-- NOTE: We could do this with a single handle per object but
	-- by maintaining our own read position and seeking before reading.
	local handle = sndfile:new(self.filename, "SFM_READ",
	                           self.samplerate, self.channel_no, self.format)

	-- Make sure that we are still reading the same file;
	-- at least with the same meta-data.
	-- Theoretically, the file could have changed since object
	-- construction.
	assert(handle.info.channels == self.channel_no and
	       handle.info.frames == self.frames,
	       "Sndfile changed")

	if self.channel_no == 1 then
		local read = handle.read

		return function()
			return read(handle)
		end
	else
		-- For multi-channel audio files, we generate a stream
		-- of frame buffers.
		-- However, the user never sees these since they are translated
		-- to a MuxStream automatically (see ctor())
		local readf = handle.readf
		local frame = sndfile.frame_type(self.channel_no)

		return function()
			return readf(handle, frame) and frame or nil
		end
	end
end

function SndfileStream:len() return self.frames end

-- TODO: Use a buffer to improve perfomance (e.g. 1024 samples)
function Stream:save(filename, format)
	if self:len() == math.huge then
		error("Cannot save infinite stream")
	end

	local channels = self.channels
	local hnd = sndfile:new(filename, "SFM_WRITE",
	                        samplerate, channels, format)

	local frame_buffer = sndfile.frame_type(channels)

	self:foreach(function(frame)
		-- NOTE: This should be (hopefully) automatically
		-- unrolled for single-channel streams
		-- Otherwise each loop copies an entire frame.
		-- This should be faster than letting LuaJIT translate
		-- the frame directly.
		for i = 1, channels do
			local sample = tonumber(frame[i])
			assert(sample ~= nil)
			frame_buffer[i-1] = sample
		end

		-- NOTE: Apparently we cannot use hnd:write() if a frame is larger than one sample
		-- (i.e. multichannel streams)
		-- FIXME: Check return value
		hnd:writef(frame_buffer)
	end)

	hnd:close()
end
