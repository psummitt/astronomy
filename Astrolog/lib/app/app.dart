import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../core/providers/theme_provider.dart';
import 'router.dart';
import 'theme.dart';

class AstroLogApp extends ConsumerWidget {
  const AstroLogApp({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final themeModeSetting = ref.watch(themeProvider);

    ThemeData theme;
    switch (themeModeSetting) {
      case AppThemeMode.light:
        theme = AppTheme.lightTheme;
        break;
      case AppThemeMode.dark:
        theme = AppTheme.darkTheme;
        break;
      case AppThemeMode.redNightVision:
        theme = AppTheme.redNightTheme;
        break;
    }

    return MaterialApp.router(
      title: 'AstroLog',
      debugShowCheckedModeBanner: false,
      theme: theme,
      darkTheme: AppTheme.darkTheme,
      themeMode: themeModeSetting == AppThemeMode.light
          ? ThemeMode.light
          : ThemeMode.dark,
      routerConfig: router,
    );
  }
}
