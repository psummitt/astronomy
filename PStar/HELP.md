# PStar Help Guide

## Introduction
PStar is a Flutter-based conversion of the classic Polaris Positions BASIC program by Eric Burgess. It calculates the times of elongations and transits of Polaris for any given date and longitude. This information is crucial for accurately aligning equatorial mounts for telescopes.

## How to Use the App

### 1. Date Selection
- Tap on the **Date** field to open the calendar picker.
- Select the date for which you want to calculate Polaris positions.

### 2. Longitude Input
- Enter your local **Longitude** in the text field.
- Longitude is typically entered in decimal degrees. 
- *Note:* The original formula's convention for East/West should be followed. Generally, positive values are East and negative values are West, but check your local sidereal time offset if results seem unexpected.

### 3. Select Event Type
Choose one of the following events:
- **West Elongation Next**: The next time Polaris reaches its furthest point West.
- **West Elongation Previous**: The most recent time Polaris reached its furthest point West.
- **East Elongation Next**: The next time Polaris reaches its furthest point East.
- **East Elongation Previous**: The most recent time Polaris reached its furthest point East.
- **Lower Transit Next**: The next time Polaris passes the lower meridian.
- **Upper Transit Next**: The next time Polaris passes the upper meridian.

### 4. Calculate
- Tap the **Calculate** button.
- The result will appear at the bottom of the screen, showing both decimal hours and the time in Hours (HR) and Minutes (MI).

## Accessibility Features
- **Screen Reader Support**: All elements are labeled for screen readers (TalkBack/VoiceOver).
- **High Contrast**: The app uses a dark theme by default for better visibility in low-light (stargazing) conditions.
- **Large Text Support**: The layout is responsive to system font size changes.
- **Keyboard Navigation**: All inputs are accessible via hardware keyboards or assistive devices.

## Accuracy
The program provides results with an accuracy of approximately 10 minutes, which is sufficient for most telescope alignment procedures.

## Credits
- **Original Author**: Eric Burgess F.R.A.S.
- **Publisher**: S & T Software Service (1980).
- **Converted to Flutter 7/6/2026**
