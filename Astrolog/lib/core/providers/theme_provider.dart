import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../../app/theme.dart';

class ThemeNotifier extends StateNotifier<AppThemeMode> {
  ThemeNotifier() : super(AppThemeMode.dark);

  void setThemeMode(AppThemeMode mode) {
    state = mode;
  }
}

final themeProvider = StateNotifierProvider<ThemeNotifier, AppThemeMode>((ref) {
  return ThemeNotifier();
});
