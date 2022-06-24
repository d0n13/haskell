# Haskell project for batch-43
This project is simulating a controller for an electric vehicle with trusters.

<img width="480" alt="Sim Window" src="https://user-images.githubusercontent.com/5494992/175504960-434d4b52-624c-45d4-86e3-475758a05f74.png">

Controls for simulated joystick and arm button are shown on screen.

### Joystick
Simulated joystick controls the direction and rotation of the trusters. The trusters will do nothing unless armed.  
- Joystick Y axis controls power to truster
- Joystick X asix control truster rotation

### Arm function
The 'arm' key switches the unit on and off. If battery drops below 5% (configurable) the controller disarms automatically. The colour of the arm indicator reflects the state.

### Truster power level
Power levels to the truster can be limited to Low, Medium, and High. As the battery drops the power to the trusters reduces automatically.
- Battery above 70%           - no truster limit
- Battery between 25 and 70%  - truster limited to 70% power
- Battery below 25%           - truster limited to 25% power

Joystick y-axis (truster power) is mapped into the power limit range. So 100% power input from joystick maps to 25% power in low power limit mode.

### Truster rotation
The trusters can rotate to control the direction of the unit. The trusters are configured to rotate within certain limit. The joystick x-axis controsl the movement. 
- Center stick position has the truster direction pointing at 270 degrees (looking from the top of the vehicle)
- Max axis right moves to the configured turn rate (ie: 45 degrees) so 270 + 45
- Max axis left moves to the configured turn rate (ie: -45 degrees) so 270 - 45

### Battery management
In this simulator the battery reduces every battery tick (3 seconds). The truster power limit drops as the battery crosses its set points. 
- Initially reduces by 4% every tick unit it hits 70% battery level
- Drops by 2% until it reaches 25% battery level
- Drops by 1% below 25% battery level 
- Controller will disarm automatically at the configured low batter limit
- Battery power graph changes from green, yellow, red as it drops


