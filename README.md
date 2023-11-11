# Applause 2

## Installation

You have to manually build and install LuaJIT v2.1:

    git clone -b v2.1 https://luajit.org/git/luajit.git
    cd luajit
    make
    sudo make install

Furthermore, install the following dependencies:

    sudo apt-get install build-essential libreadline-dev libjack-jackd2-dev \
                         libsndfile1 libasound2 feedgnuplot

To compile the project, type:

    make

Up-to-date documentation is available at the [website](http://rhaberkorn.github.io/applause2).
In case you want to build it manually, install the `lua-ldoc` and `lua-discount` packages and type:

    make doc

The generated documentation will be generated in the `doc/` subdirectory.

## Usage

Start qjackctl.

TODO: How to use jack-plumbing?

    ./applause -o 2

This may require root rights for accessing HID devices.
You may also add the current user to the `input` group.

Example (one channel):

    > Stream.SinOsc(440):play()

You can also run standalone scripts (batch mode), just like the standard Lua interpreter.

# Applause Clients (Editor Integration)

    echo -ne "25   \nStream.SinOsc(440):play()" | socat -,ignoreeof TCP:127.0.0.1:10000"

See also `client.tes` for a [SciTECO](https://github.com/rhaberkorn/sciteco) integration.

# Joysticks and Gamepads

This is supported by EvdevStream().

Alternatively you can use aseqjoy together with `a2jmidid --export-hw`
to expose them as MIDI events.

# Mice

This is supported by EvdevStream().

Alternatively you can use [raton](https://github.com/GModal/raton) together with `a2jmidid --export-hw`
to expose them as MIDI events.

# Other useful programs

* jack_rec, QJackRcd or Audacity to record sessions
* midisnoop for diplaying MIDI events
* jack-keyboard for producing MIDI note events
* [midicontroller](https://sourceforge.net/projects/midicontrol/) for producing MIDI CC events
* MIDI Tracker ???
* evtest to find and test HID devices
* listplugins and analyseplugin to inspect LADSPA plugins
* dssi_list_plugins and dssi_analyse_plugin to inspect DSSI plugins
