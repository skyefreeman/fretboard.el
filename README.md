# 🎸 fretboard.el

An Emacs package for visualizing notes, scales and chords on a guitar fretboard.

![Fretboard usage example](https://raw.githubusercontent.com/skyefreeman/fretboard.el/main/examples/fretboard-example-usage.gif)

## Features

- Display scales, chords and modes on a guitar fretboard.
- Cycle between chord tonalities.
- Toggle between the 7 diatonic modes.
- View the fretboard notes by either name or intervals.
- Support for multiple tunings
- Interactive navigation mode.

## Installation

### Manual

1. Download or clone the `fretboard.el` repo onto your local machine.
2. Add the following to your Emacs configuration:

```elisp
;; Add the directory containing fretboard.el to your load-path
(add-to-list 'load-path "/path/to/directory/containing/fretboard.el")
(require 'fretboard)
```

### Straight

```elisp
(use-package fretboard
  :straight (:host github :repo "skyefreeman/fretboard.el")
  :defer t)
```

### Elpaca

```elisp
(use-package fretboard
  :ensure (:host github :repo "skyefreeman/fretboard.el")
  :defer t)
```

### Doom

```elisp
(package! fretboard
  :recipe (:type git :host github :repo "skyefreeman/fretboard.el" :branch "main" :files ("*.el"))
  :defer t)
```

### package.el

```elisp
(use-package fretboard
  :vc (:fetcher github :repo skyefreeman/fretboard.el)
  :defer t)
```

## Usage

### Basic Commands

- `M-x fretboard` - Display the A major scale on the fretboard (default)
- `M-x fretboard-display-scale` - Display a scale on the fretboard
- `M-x fretboard-display-chord` - Display a chord on the fretboard
- `M-x fretboard-set-tuning` - Change the guitar tuning

### Customization

You can customize the appearance and behavior of fretboard.el:

```elisp
;; Change the number of frets displayed (default is 12)
(setq fretboard-fret-count 24)
```

### Navigation

When viewing a fretboard display, you can use the following keys:

| Key | Function |
|-----|----------|
| `n` | Next note (same scale/chord type) |
| `p` | Previous note (same scale/chord type) |
| `k` | Next scale/chord type (same note) |
| `j` | Previous scale/chord type (same note) |
| `,` | Next mode (same scale, only for major scale) |
| `m` | Previous mode (same scale, only for major scale) |
| `d` | Toggle between scale and chord display |
| `t` | Toggle between different tunings |
| `r` | Toggle between note names and relative intervals |
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

## Available Modes

When viewing the major scale you can toggle between the 7 diatonic [modes](https://en.wikipedia.org/wiki/Mode_(music)). 

- Ionian (Major)
- Dorian
- Phrygian
- Lydian
- Mixolydian
- Aeolian (Natural Minor)
- Locrian

## Available Tunings

- Standard (E A D G B E)
- Drop D (D A D G B E)
- Open G (D G D G B D)
- DADGAD (D A D G A D)
- Half step down (Eb Ab Db Gb Bb Eb)

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation.
