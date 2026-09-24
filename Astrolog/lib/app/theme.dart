import 'package:flutter/material.dart';

enum AppThemeMode {
  light,
  dark,
  redNightVision,
}

class AppTheme {
  static ThemeData get lightTheme {
    return ThemeData(
      useMaterial3: true,
      colorScheme: ColorScheme.fromSeed(
        seedColor: const Color(0xFF0D47A1), // Deep Blue
        brightness: Brightness.light,
      ),
      focusColor: const Color(0xFF0D47A1).withValues(alpha: 0.2),
      visualDensity: VisualDensity.adaptivePlatformDensity,
      cardTheme: const CardThemeData(
        elevation: 2,
        margin: EdgeInsets.symmetric(horizontal: 16, vertical: 8),
      ),
      iconButtonTheme: IconButtonThemeData(
        style: IconButton.styleFrom(
          minimumSize: const Size(48, 48),
        ),
      ),
    );
  }

  static ThemeData get darkTheme {
    return ThemeData(
      useMaterial3: true,
      colorScheme: ColorScheme.fromSeed(
        seedColor: const Color(0xFF1565C0), // Deep Space Blue
        brightness: Brightness.dark,
        surface: const Color(0xFF121212),
      ),
      focusColor: Colors.lightBlueAccent.withValues(alpha: 0.25),
      visualDensity: VisualDensity.adaptivePlatformDensity,
      cardTheme: const CardThemeData(
        elevation: 3,
        margin: EdgeInsets.symmetric(horizontal: 16, vertical: 8),
      ),
      iconButtonTheme: IconButtonThemeData(
        style: IconButton.styleFrom(
          minimumSize: const Size(48, 48),
        ),
      ),
    );
  }

  /// Red Night Vision theme designed for field astronomy to preserve dark adaptation (scotopic vision).
  /// High-contrast red-on-black UI complying with WCAG 2.2 AA standards.
  static ThemeData get redNightTheme {
    const redPrimary = Color(0xFFFF2200);
    const redBackground = Color(0xFF000000);
    const redSurface = Color(0xFF150000);
    const redOnSurface = Color(0xFFFF6666);

    const colorScheme = ColorScheme.dark(
      primary: redPrimary,
      onPrimary: Colors.black,
      secondary: Color(0xFFFF4444),
      onSecondary: Colors.black,
      surface: redSurface,
      onSurface: redOnSurface,
      error: Color(0xFFFF0000),
      onError: Colors.black,
    );

    return ThemeData(
      useMaterial3: true,
      brightness: Brightness.dark,
      scaffoldBackgroundColor: redBackground,
      colorScheme: colorScheme,
      focusColor: redPrimary.withValues(alpha: 0.4),
      dialogTheme: const DialogThemeData(
        backgroundColor: redSurface,
      ),
      cardTheme: const CardThemeData(
        color: redSurface,
        elevation: 4,
        margin: EdgeInsets.symmetric(horizontal: 16, vertical: 8),
      ),
      appBarTheme: const AppBarTheme(
        backgroundColor: redSurface,
        foregroundColor: redPrimary,
        elevation: 0,
      ),
      navigationRailTheme: const NavigationRailThemeData(
        backgroundColor: redSurface,
        selectedIconTheme: IconThemeData(color: redPrimary),
        unselectedIconTheme: IconThemeData(color: redOnSurface),
        selectedLabelTextStyle: TextStyle(color: redPrimary, fontWeight: FontWeight.bold),
        unselectedLabelTextStyle: TextStyle(color: redOnSurface),
      ),
      navigationBarTheme: NavigationBarThemeData(
        backgroundColor: redSurface,
        indicatorColor: redPrimary.withValues(alpha: 0.3),
        iconTheme: WidgetStateProperty.resolveWith((states) {
          if (states.contains(WidgetState.selected)) {
            return const IconThemeData(color: redPrimary);
          }
          return const IconThemeData(color: redOnSurface);
        }),
      ),
      elevatedButtonTheme: ElevatedButtonThemeData(
        style: ElevatedButton.styleFrom(
          backgroundColor: redPrimary,
          foregroundColor: Colors.black,
          minimumSize: const Size(88, 48),
        ),
      ),
      iconButtonTheme: IconButtonThemeData(
        style: IconButton.styleFrom(
          minimumSize: const Size(48, 48),
        ),
      ),
    );
  }
}
