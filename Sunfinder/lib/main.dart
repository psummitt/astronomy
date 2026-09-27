import 'package:flutter/material.dart';
import 'home_screen.dart';
import 'settings_model.dart';
import 'classic_mode_screen.dart';

void main() async {
  WidgetsFlutterBinding.ensureInitialized();
  final settings = await SunfinderSettings.load();
  runApp(SunfinderApp(settings: settings));
}

class SunfinderApp extends StatefulWidget {
  final SunfinderSettings settings;
  const SunfinderApp({super.key, required this.settings});

  @override
  State<SunfinderApp> createState() => _SunfinderAppState();

  static _SunfinderAppState of(BuildContext context) => 
      context.findAncestorStateOfType<_SunfinderAppState>()!;
}

class _SunfinderAppState extends State<SunfinderApp> {
  late SunfinderSettings settings;

  @override
  void initState() {
    super.initState();
    settings = widget.settings;
  }

  void updateSettings(SunfinderSettings newSettings) {
    setState(() {
      settings = newSettings;
    });
    settings.save();
  }

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Sunfinder',
      theme: ThemeData(
        useMaterial3: true,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.amber,
          brightness: Brightness.light,
        ),
      ),
      darkTheme: ThemeData(
        useMaterial3: true,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.amber,
          brightness: Brightness.dark,
        ),
      ),
      home: settings.isModernMode 
          ? HomeScreen(settings: settings) 
          : ClassicModeScreen(settings: settings),
    );
  }
}
