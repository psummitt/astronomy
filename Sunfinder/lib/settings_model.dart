import 'package:shared_preferences/shared_preferences.dart';

class SunfinderSettings {
  double latitude;
  double longitude;
  double standardMeridian;
  double magneticVariation;
  bool isLeapYear;
  bool isModernMode;

  SunfinderSettings({
    this.latitude = 0.0,
    this.longitude = 0.0,
    this.standardMeridian = 0.0,
    this.magneticVariation = 0.0,
    this.isLeapYear = false,
    this.isModernMode = true,
  });

  static Future<SunfinderSettings> load() async {
    final prefs = await SharedPreferences.getInstance();
    return SunfinderSettings(
      latitude: prefs.getDouble('latitude') ?? 0.0,
      longitude: prefs.getDouble('longitude') ?? 0.0,
      standardMeridian: prefs.getDouble('standardMeridian') ?? 0.0,
      magneticVariation: prefs.getDouble('magneticVariation') ?? 0.0,
      isLeapYear: prefs.getBool('isLeapYear') ?? false,
      isModernMode: prefs.getBool('isModernMode') ?? true,
    );
  }

  Future<void> save() async {
    final prefs = await SharedPreferences.getInstance();
    await prefs.setDouble('latitude', latitude);
    await prefs.setDouble('longitude', longitude);
    await prefs.setDouble('standardMeridian', standardMeridian);
    await prefs.setDouble('magneticVariation', magneticVariation);
    await prefs.setBool('isLeapYear', isLeapYear);
    await prefs.setBool('isModernMode', isModernMode);
  }
}
