trackpoint = EvdevStream("TrackPoint")
trackpoint:evrel('REL_X'):scale(440,880):SinOsc():gain(trackpoint:evrel('REL_Y'):scale(1)):play()

-- This is for the trackball and makes nice noises probably due to overflowing the Quality value
trackball = EvdevStream("TrackBall Mouse")
NoiseStream:BPF(trackball:evrel('REL_X'):scale(100,5000), trackball:evrel('REL_Y')):gain(trackball:evrel('REL_WHEEL'):scale(1)):play()

touchpad = EvdevStream("TouchPad")
touchpad:evabs('ABS_X', 1232, 5712):scale(440,880):SinOsc():gain(touchpad:evabs('ABS_Y', 1074, 4780):scale(1)):play()

-- FIXME: Make a small polyphonic keyboard
EvdevStream(10):evkey(16):instrument(Stream.SinOsc(440)):play()
