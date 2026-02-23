# Hew for Sublime Text

Syntax highlighting for the [Hew](https://hew.sh) programming language in Sublime Text.

## Installation

### Manual

1. Open Sublime Text
2. Go to **Preferences → Browse Packages…** to open the packages directory
3. Create a `Hew` folder inside the packages directory
4. Copy the following files into the `Hew` folder:
   - `Hew.tmLanguage.json`
   - `Comments.tmPreferences`

### From this repository

```sh
# macOS
cp editors/sublime/*.{json,tmPreferences} ~/Library/Application\ Support/Sublime\ Text/Packages/Hew/

# Linux
cp editors/sublime/*.{json,tmPreferences} ~/.config/sublime-text/Packages/Hew/

# Windows (PowerShell)
Copy-Item editors/sublime/*.json,editors/sublime/*.tmPreferences "$env:APPDATA\Sublime Text\Packages\Hew\"
```

## Features

- Full syntax highlighting for Hew keywords, types, actors, strings, comments, and literals
- `//` and `/* */` comment toggling via Ctrl+/ (Cmd+/ on macOS)
- Automatic `.hew` file association
