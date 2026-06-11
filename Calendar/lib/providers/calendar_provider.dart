import 'package:flutter/foundation.dart';
import 'package:shared_preferences/shared_preferences.dart';
import '../models/holiday.dart';
import '../utils/calendar_engine.dart';

class CalendarProvider with ChangeNotifier {
  int _selectedYear = DateTime.now().year;
  int _selectedMonth = DateTime.now().month;
  String _themeMode = 'modern'; // 'modern' or 'retro'
  
  Set<String> _disabledHolidayIds = {};
  List<Holiday> _customHolidays = [];
  bool _isLoaded = false;

  int get selectedYear => _selectedYear;
  int get selectedMonth => _selectedMonth;
  String get themeMode => _themeMode;
  bool get isLoaded => _isLoaded;

  Set<String> get disabledHolidayIds => _disabledHolidayIds;
  List<Holiday> get customHolidays => _customHolidays;

  CalendarProvider() {
    _loadFromPrefs();
  }

  // Get active holidays for the selected year
  List<Holiday> getHolidaysForSelectedYear() {
    List<Holiday> yearHolidays = [];
    
    // Standard holidays for the selected year
    final standard = CalendarEngine.calculateStandardHolidays(_selectedYear);
    for (var h in standard) {
      final isEnabled = !_disabledHolidayIds.contains(h.id);
      yearHolidays.add(h.copyWith(isEnabled: isEnabled));
    }

    // Custom holidays
    for (var h in _customHolidays) {
      if (h.year == null || h.year == _selectedYear) {
        yearHolidays.add(h);
      }
    }

    // Sort by month then day
    yearHolidays.sort((a, b) {
      if (a.month != b.month) {
        return a.month.compareTo(b.month);
      }
      return a.day.compareTo(b.day);
    });

    return yearHolidays;
  }

  // Get map of day -> holiday names for the selected year/month grid highlighting
  Map<int, List<Holiday>> getHolidaysForSelectedMonth() {
    final list = getHolidaysForSelectedYear();
    Map<int, List<Holiday>> monthMap = {};
    for (var h in list) {
      if (h.month == _selectedMonth && h.isEnabled) {
        monthMap.putIfAbsent(h.day, () => []).add(h);
      }
    }
    return monthMap;
  }

  void setYear(int year) {
    if (year >= 1753) {
      _selectedYear = year;
      notifyListeners();
      _saveToPrefs();
    }
  }

  void setMonth(int month) {
    if (month >= 1 && month <= 12) {
      _selectedMonth = month;
      notifyListeners();
      _saveToPrefs();
    }
  }

  void toggleTheme() {
    _themeMode = _themeMode == 'modern' ? 'retro' : 'modern';
    notifyListeners();
    _saveToPrefs();
  }

  void setTheme(String mode) {
    if (mode == 'modern' || mode == 'retro') {
      _themeMode = mode;
      notifyListeners();
      _saveToPrefs();
    }
  }

  void toggleHolidayEnabled(String id, bool enabled) {
    final isCustom = _customHolidays.any((h) => h.id == id);
    if (isCustom) {
      _customHolidays = _customHolidays.map((h) {
        if (h.id == id) {
          return h.copyWith(isEnabled: enabled);
        }
        return h;
      }).toList();
    } else {
      if (enabled) {
        _disabledHolidayIds.remove(id);
      } else {
        _disabledHolidayIds.add(id);
      }
    }
    notifyListeners();
    _saveToPrefs();
  }

  void addCustomHoliday(Holiday holiday) {
    _customHolidays.add(holiday);
    notifyListeners();
    _saveToPrefs();
  }

  void deleteCustomHoliday(String id) {
    _customHolidays.removeWhere((h) => h.id == id);
    notifyListeners();
    _saveToPrefs();
  }

  Future<void> _loadFromPrefs() async {
    try {
      final prefs = await SharedPreferences.getInstance();
      _selectedYear = prefs.getInt('selectedYear') ?? DateTime.now().year;
      _selectedMonth = prefs.getInt('selectedMonth') ?? DateTime.now().month;
      _themeMode = prefs.getString('themeMode') ?? 'modern';
      
      final disabledList = prefs.getStringList('disabledHolidayIds') ?? [];
      _disabledHolidayIds = Set.from(disabledList);

      final customJsonList = prefs.getStringList('customHolidays') ?? [];
      _customHolidays = customJsonList
          .map((jsonStr) => Holiday.fromJson(jsonStr))
          .toList();
    } catch (e) {
      debugPrint("Error loading from SharedPreferences: $e");
    } finally {
      _isLoaded = true;
      notifyListeners();
    }
  }

  Future<void> _saveToPrefs() async {
    try {
      final prefs = await SharedPreferences.getInstance();
      await prefs.setInt('selectedYear', _selectedYear);
      await prefs.setInt('selectedMonth', _selectedMonth);
      await prefs.setString('themeMode', _themeMode);
      await prefs.setStringList('disabledHolidayIds', _disabledHolidayIds.toList());
      
      final customJsonList = _customHolidays
          .map((h) => h.toJson())
          .toList();
      await prefs.setStringList('customHolidays', customJsonList);
    } catch (e) {
      debugPrint("Error saving to SharedPreferences: $e");
    }
  }
}
