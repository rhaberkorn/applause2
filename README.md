# Applause 2

## Installation

You have to manually build and install LuaJIT v2.1:

    git clone -b v2.1 https://luajit.org/git/luajit.git
    cd luajit
    make
    sudo make install

Furthermore, install the following dependencies (Ubuntu):

    sudo apt-get install build-essential libreadline-dev libjack-jackd2-dev \
                         libsndfile1 libasound2 feedgnuplot

On FreeBSD, you will need the following packages/ports:

    pkg install gmake readline jackit evdev-proto libsndfile alsa-lib p5-feedgnuplot

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

Example (one channel):

    > Stream.SinOsc(440):play()

You can also run standalone scripts (batch mode), just like the standard Lua interpreter.

## Operating System Tweaks

### Linux (Ubuntu)

In order to run Jack and Applause with real-time scheduling, it should be sufficient to
add your user to the `audio` group.

To give regular users access to HID devices, it should suffice to add the current user to
the `input` group.

### FreeBSD

For realtime scheduling, you might have to check out the mac_priority kernel module
and add your user to the `realtime` group.

Furthermore, to allow unlimited memory locking on FreeBSD for ordinary users,
you should add the following entry to `/etc/login.conf`:

    audio:\
    	:memorylocked=unlimited:\
    	:tc=default:

Change the login class of your user to `audio` by running `chpass`.

You might need to add the current user to the `wheel` group and
give read acceess to evdev device nodes by creating `/etc/devd.rules`:

    [localrules=10]
      add path 'input/*' mode 0640

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

* jack_rec, jack-capture, [QJackRcd](https://orouits.github.io/qjackrcd/) or [Audacity](https://www.audacityteam.org/) to record sessions
* jack_midi_dump or [midisnoop](https://github.com/surfacepatterns/midisnoop) for diplaying MIDI events
* [jack-keyboard](https://jack-keyboard.sourceforge.net/) for producing MIDI note events
* [midicontroller](https://sourceforge.net/projects/midicontrol/) for producing MIDI CC events
* MIDI Tracker ???
* [rtspeccy](https://www.uninformativ.de/git/rtspeccy) for a realtime spectrogram
* evtest to find and test HID devices
* listplugins and analyseplugin to inspect LADSPA plugins
* dssi_list_plugins and dssi_analyse_plugin to inspect DSSI plugins

## Jupyter Console and Notebook

Applause can be run in [Jupyter](https://jupyter.org/) Consoles and even Notebooks thanks to
[ILua](https://github.com/guysv/ilua).
For full support of all feautures, you must currently use an unofficial [ILua fork](https://github.com/rhaberkorn/ilua).
First, install ILua into a Python environment
(see also this [ILua ticket](https://github.com/guysv/ilua/issues/28)):

```bash
git clone -b improvements --recurse-submodules https://github.com/rhaberkorn/ilua.git
cd ilua
python3 -m venv env
. env/bin/activate
pip install twisted==22.10.0 .
```

You can now directly run an Applause Jupyter Console session:

```bash
cd ~/applause
ilua --lua-interpreter=./applause
```

In order to tweak Applause command line parameters and be independant of the execution directory, use
the included wrapper script.
It also allows passing in additional arguments to Applause, e.g.:

```bash
APPLAUSE_OPTS="-o 2" ilua --lua-interpreter=./ilua-wrapper.sh
```

You can symlink this to `lua` in the Python environment to make Applause the default
ILua interpreter in this Python environment:

```bash
ln -s ~/applause/ilua-wrapper.sh env/bin/lua
```

If you would like to launch a Jupyter Notebook (Web UI!), first install the following Pip package:

```bash
pip install notebook
```

Now launch a web server and follow the onscreen instructions:

```bash
APPLAUSE_OPTS="-o 2" jupyter notebook --MultiKernelManager.default_kernel_name=lua
```

This works assuming that you symlinked `ilua-wrapper.sh` to `lua` as described above.
An alternative might be to create a custom Jupyter kernel configuration (kernel.json).

If the browser is not opened automatically on the notebook's URL, you might want to try
visiting http://localhost:8888/.

Please note the following restrictions/bugs:

* You cannot publicly host the Jupyter Notebook as the sound is generated on the host machine.
  Similarily, it would be tricky to wrap everything in a Docker container.
* The output of some functions like Stream:toplot() is garbled.
