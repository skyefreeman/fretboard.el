# fretboard.el

An Emacs package for visualizing notes, scales and chords on a guitar fretboard.

## Features

- Display scales and chords on a guitar fretboard
- Navigate through different notes and scale/chord types
- Support for multiple tunings
- Interactive navigation mode

## Installation

### Manual Installation

1. Download `fretboard.el` to your local machine
2. Add the following to your Emacs configuration:

```elisp
;; Add the directory containing fretboard.el to your load-path
(add-to-list 'load-path "/path/to/directory/containing/fretboard.el")
(require 'fretboard)
```

## Usage

### Basic Commands

- `M-x fretboard` - Display the A major scale on the fretboard (default)
- `M-x fretboard-display-scale` - Display a scale on the fretboard
- `M-x fretboard-display-chord` - Display a chord on the fretboard
- `M-x fretboard-set-tuning` - Change the guitar tuning

### Navigation

When viewing a fretboard display, you can use the following keys:

| Key | Function |
|-----|----------|
| `n` | Next note (same scale/chord type) |
| `p` | Previous note (same scale/chord type) |
| `k` | Next scale/chord type (same note) |
| `j` | Previous scale/chord type (same note) |
| `d` | Toggle between scale and chord display |
| `t` | Toggle between different tunings |
| `s` | Switch to scale display |
| `c` | Switch to chord display |
| `q` | Close all fretboard buffers |

## Available Scales

- Major
- Minor
- Pentatonic Major
- Pentatonic Minor
- Blues
- Harmonic Minor
- Melodic Minor

## Available Chords

- Major
- Minor
- 7th
- Major 7th
- Minor 7th
- Diminished
- Augmented
- Sus2
- Sus4
- Add9
- 6th
- Minor 6th
- 9th
- Minor 9th

## Available Tunings

- Standard (E A D G B E)
- Drop D (D A D G B E)
- Open G (D G D G B D)
- DADGAD (D A D G A D)
- Half step down (Eb Ab Db Gb Bb Eb)

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation.
