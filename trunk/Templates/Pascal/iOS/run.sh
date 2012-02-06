echo "  ... Starting iOS Simulator."
#restart simulator
osascript <<EOF
tell application "System Events" 
  set SimulatorOpen to ("iPhone Simulator") 
end tell
tell application "iPhone Simulator"
  if (SimulatorOpen contains "iPhone Simulator") is true then 
      quit
      delay 1
    end if
    activate
end tell
EOF