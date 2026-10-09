import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../core/providers/theme_provider.dart';
import 'router.dart';
import 'theme.dart';

class AstroDiaryApp extends ConsumerWidget {
  const AstroDiaryApp({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final themeMode = ref.watch(themeProvider);

    ThemeData currentTheme;
    switch (themeMode) {
      case AppThemeMode.light:
        currentTheme = AppTheme.lightTheme;
        break;
      case AppThemeMode.dark:
        currentTheme = AppTheme.darkTheme;
        break;
      case AppThemeMode.redNightVision:
        currentTheme = AppTheme.redNightTheme;
        break;
    }

    return MaterialApp.router(
      title: 'AstroDiary',
      debugShowCheckedModeBanner: false,
      theme: currentTheme,
      routerConfig: router,
    );
  }
}
