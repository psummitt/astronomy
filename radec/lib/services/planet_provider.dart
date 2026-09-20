import 'package:flutter/foundation.dart';
import '../models/planet_data.dart';
import 'local_calculator.dart';
import 'remote_service.dart';

enum CalculationMode { local, remote }

class PlanetProvider with ChangeNotifier {
  List<PlanetData> _planets = [];
  bool _isLoading = false;
  String? _errorMessage;
  CalculationMode _mode = CalculationMode.local;
  DateTime _selectedDate = DateTime.now();

  List<PlanetData> get planets => _planets;
  bool get isLoading => _isLoading;
  String? get errorMessage => _errorMessage;
  CalculationMode get mode => _mode;
  DateTime get selectedDate => _selectedDate;

  final RemoteService _remoteService = RemoteService();

  void setMode(CalculationMode mode) {
    _mode = mode;
    calculate();
  }

  void setDate(DateTime date) {
    _selectedDate = date;
    calculate();
  }

  Future<void> calculate() async {
    _isLoading = true;
    _planets = [];
    _errorMessage = null;
    notifyListeners();

    try {
      if (_mode == CalculationMode.local) {
        _planets = LocalCalculator.calculate(_selectedDate);
      } else {
        _planets = await _remoteService.fetchPlanetData(_selectedDate);
        
        // Smart Fallback: Fill in gaps and estimates
        final estimates = LocalCalculator.calculate(_selectedDate);
        
        // 1. Ensure all planets from the calculator are represented
        for (var estimate in estimates) {
          int index = _planets.indexWhere((p) => p.name == estimate.name);
          if (index == -1) {
            // Planet missing entirely (e.g. Pluto in Visible Planets fallback)
            _planets.add(estimate);
          } else if (_planets[index].distanceToSun == 0.0 || _planets[index].distanceToEarth == 0.0) {
            // Distance missing, fill from estimate
            _planets[index] = PlanetData(
              name: _planets[index].name,
              rightAscension: _planets[index].rightAscension,
              declination: _planets[index].declination,
              distanceToSun: estimate.distanceToSun,
              distanceToEarth: estimate.distanceToEarth,
            );
          }
        }

        // Sort by standard solar system order
        const order = ['Mercury', 'Venus', 'Mars', 'Jupiter', 'Saturn', 'Uranus', 'Neptune', 'Pluto'];
        _planets.sort((a, b) => order.indexOf(a.name).compareTo(order.indexOf(b.name)));

        if (kIsWeb && _planets.isNotEmpty) {
          // If we had to rely on estimates for everything, show a note
          bool allEstimates = _planets.every((p) => p.distanceToSun != 0.0); // Wait, this logic is tricky
          // Actually, just show the error message if NASA failed completely
        }

        if (_planets.isEmpty) {
          if (kIsWeb) {
            _errorMessage = "Note: Browser security (CORS) may be blocking high-precision NASA data. Showing local estimates.";
            _planets = estimates;
          } else {
            _errorMessage = "No data returned from API. Please check your internet connection.";
          }
        }
      }
    } catch (e) {
      _errorMessage = "An error occurred: $e";
    }

    _isLoading = false;
    notifyListeners();
  }
}
