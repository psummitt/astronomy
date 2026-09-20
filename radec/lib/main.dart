import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'services/planet_provider.dart';
import 'ui/home_screen.dart';

void main() {
  runApp(
    ChangeNotifierProvider(
      create: (context) => PlanetProvider()..calculate(),
      child: const RadecApp(),
    ),
  );
}

class RadecApp extends StatelessWidget {
  const RadecApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Radec Astronomy',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.indigo),
        useMaterial3: true,
        visualDensity: VisualDensity.adaptivePlatformDensity,
      ),
      home: const HomeScreen(),
    );
  }
}
