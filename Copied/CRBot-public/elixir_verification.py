# TO MAKE SURE BLUESTACKS IS ALIGNED FOR PROPER ELIXIR COUNTING!
# This script checks the pixel color at specific coordinates in BlueStacks to verify elixir counts.
import pyautogui
target = (225, 128, 229)
tolerance = 80
while True:
    count = 0
    for x in range(1512, 1892, 38):
        r, g, b = pyautogui.pixel(x, 989)
        if (abs(r - target[0]) <= tolerance) and (abs(g - target[1]) <= tolerance) and (abs(b - target[2]) <= tolerance):
            count += 1
    print(count)