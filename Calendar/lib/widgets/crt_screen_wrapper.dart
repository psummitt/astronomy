import 'dart:math' as math;
import 'package:flutter/material.dart';

class CrtScreenWrapper extends StatefulWidget {
  final Widget child;
  final bool enableRetro;

  const CrtScreenWrapper({
    super.key,
    required this.child,
    this.enableRetro = true,
  });

  @override
  State<CrtScreenWrapper> createState() => _CrtScreenWrapperState();
}

class _CrtScreenWrapperState extends State<CrtScreenWrapper>
    with SingleTickerProviderStateMixin {
  late AnimationController _controller;
  final math.Random _random = math.Random();

  @override
  void initState() {
    super.initState();
    // Flicker animation controller
    _controller = AnimationController(
      vsync: this,
      duration: const Duration(milliseconds: 100),
    )..repeat(reverse: true);
  }

  @override
  void dispose() {
    _controller.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    if (!widget.enableRetro) {
      return widget.child;
    }

    final retroTheme = ThemeData(
      brightness: Brightness.dark,
      scaffoldBackgroundColor: const Color(0xFF020902),
      primaryColor: const Color(0xFF33FF33),
      colorScheme: const ColorScheme.dark(
        primary: Color(0xFF33FF33),
        onPrimary: Color(0xFF020902),
        secondary: Color(0xFF00AA00),
        onSecondary: Color(0xFF33FF33),
        surface: Color(0xFF051805),
        onSurface: Color(0xFF33FF33),
      ),
      textTheme: const TextTheme(
        bodyLarge: TextStyle(fontFamily: 'monospace', color: Color(0xFF33FF33), height: 1.2),
        bodyMedium: TextStyle(fontFamily: 'monospace', color: Color(0xFF33FF33), height: 1.2),
        titleLarge: TextStyle(
          fontFamily: 'monospace',
          color: Color(0xFF33FF33),
          fontWeight: FontWeight.bold,
          letterSpacing: 1.5,
        ),
        titleMedium: TextStyle(fontFamily: 'monospace', color: Color(0xFF33FF33)),
        labelLarge: TextStyle(fontFamily: 'monospace', color: Color(0xFF33FF33)),
      ),
      inputDecorationTheme: const InputDecorationTheme(
        enabledBorder: OutlineInputBorder(
          borderSide: BorderSide(color: Color(0xFF00AA00), width: 1.5),
        ),
        focusedBorder: OutlineInputBorder(
          borderSide: BorderSide(color: Color(0xFF33FF33), width: 2.0),
        ),
        labelStyle: TextStyle(fontFamily: 'monospace', color: Color(0xFF00AA00)),
        hintStyle: TextStyle(fontFamily: 'monospace', color: Color(0xFF024402)),
      ),
      textButtonTheme: TextButtonThemeData(
        style: TextButton.styleFrom(
          foregroundColor: const Color(0xFF33FF33),
          textStyle: const TextStyle(fontFamily: 'monospace', fontWeight: FontWeight.bold),
        ),
      ),
      elevatedButtonTheme: ElevatedButtonThemeData(
        style: ElevatedButton.styleFrom(
          foregroundColor: const Color(0xFF020902),
          backgroundColor: const Color(0xFF33FF33),
          textStyle: const TextStyle(fontFamily: 'monospace', fontWeight: FontWeight.bold),
          shape: const BeveledRectangleBorder(),
        ),
      ),
    );

    return Theme(
      data: retroTheme,
      child: Scaffold(
        body: AnimatedBuilder(
          animation: _controller,
          builder: (context, child) {
            // Add subtle random variance for realistic flicker
            final flickerVal = 0.03 + _random.nextDouble() * 0.02;

            return Stack(
              fit: StackFit.expand,
              children: [
                // 1. The actual application content
                child!,

                // 2. Phosphor screen glow overlay (flicker)
                IgnorePointer(
                  child: Container(
                    color: Color(0xFF33FF33).withValues(alpha: flickerVal),
                  ),
                ),

                // 3. Scanline overlay & Curvature vignette
                IgnorePointer(
                  child: CustomPaint(
                    painter: CrtEffectsPainter(),
                  ),
                ),

                // 4. Subtle static horizontal glare
                IgnorePointer(
                  child: Container(
                    decoration: BoxDecoration(
                      gradient: LinearGradient(
                        begin: Alignment.topCenter,
                        end: Alignment.bottomCenter,
                        colors: [
                          Colors.transparent,
                          const Color(0xFF33FF33).withValues(alpha: 0.015),
                          Colors.transparent,
                        ],
                        stops: const [0.3, 0.5, 0.7],
                      ),
                    ),
                  ),
                ),
              ],
            );
          },
          child: Container(
            padding: const EdgeInsets.all(16.0),
            color: const Color(0xFF020902),
            child: widget.child,
          ),
        ),
      ),
    );
  }
}

class CrtEffectsPainter extends CustomPainter {
  @override
  void paint(Canvas canvas, Size size) {
    final paintScanline = Paint()
      ..color = const Color(0xFF000000).withValues(alpha: 0.18)
      ..strokeWidth = 1.0;

    // Draw horizontal scanlines
    const double step = 3.0; // Draw every 3 pixels
    for (double y = 0; y < size.height; y += step) {
      canvas.drawLine(Offset(0, y), Offset(size.width, y), paintScanline);
    }

    // Vignette / screen curvature overlay
    final rect = Offset.zero & size;

    final vignettePaint = Paint()
      ..shader = RadialGradient(
        colors: [
          Colors.transparent,
          const Color(0xFF000000).withValues(alpha: 0.15),
          const Color(0xFF000000).withValues(alpha: 0.7),
        ],
        stops: const [0.6, 0.85, 1.0],
      ).createShader(rect);

    canvas.drawRect(rect, vignettePaint);
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) => false;
}
