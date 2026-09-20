import 'dart:convert';
import 'package:http/http.dart' as http;
import '../models/planet_data.dart';

class RemoteService {
  static const Map<String, String> _planetIds = {
    'Mercury': '199',
    'Venus': '299',
    'Mars': '499',
    'Jupiter': '599',
    'Saturn': '699',
    'Uranus': '799',
    'Neptune': '899',
    'Pluto': '999',
  };

  Future<List<PlanetData>> fetchPlanetData(DateTime date) async {
    final dateStr = '${date.year}-${date.month.toString().padLeft(2, '0')}-${date.day.toString().padLeft(2, '0')}';
    final nextDate = date.add(const Duration(days: 1));
    final nextDateStr = '${nextDate.year}-${nextDate.month.toString().padLeft(2, '0')}-${nextDate.day.toString().padLeft(2, '0')}';

    // 1. Fetch from NASA JPL (All data including distances)
    // We run these in parallel for speed.
    final List<Future<PlanetData?>> nasaFutures = _planetIds.entries.map((entry) {
      return _fetchNasaPlanet(entry.key, entry.value, dateStr, nextDateStr);
    }).toList();

    List<PlanetData?> nasaResults = await Future.wait(nasaFutures);
    List<PlanetData> results = nasaResults.whereType<PlanetData>().toList();

    // 2. If some planets are missing (e.g. NASA failed), try Visible Planets API as fallback for current positions
    if (results.length < _planetIds.length) {
      try {
        final vpResults = await _fetchVisiblePlanets();
        for (var vpPlanet in vpResults) {
          // If we don't already have this planet from NASA, add it
          if (!results.any((p) => p.name == vpPlanet.name)) {
            results.add(vpPlanet);
          }
        }
      } catch (_) {}
    }

    // Sort by standard solar system order
    final order = _planetIds.keys.toList();
    results.sort((a, b) => order.indexOf(a.name).compareTo(order.indexOf(b.name)));

    return results;
  }

  Future<PlanetData?> _fetchNasaPlanet(String name, String id, String start, String end) async {
    try {
      // Use Uri.parse to ensure we can control the query string precisely if needed.
      // NASA Horizons API documentation says COMMAND='499' is the format.
      // We'll use Uri.https which is cleaner and handles encoding correctly.
      final url = Uri.https('ssd.jpl.nasa.gov', '/api/horizons.api', {
        'format': 'json',
        'COMMAND': "'$id'",
        'OBJ_DATA': 'NO',
        'MAKE_EPHEM': 'YES',
        'EPHEM_TYPE': 'OBSERVER',
        'CENTER': '500@399',
        'START_TIME': start,
        'STOP_TIME': end,
        'STEP_SIZE': '1d',
        'QUANTITIES': "'1,19,20'",
        'CSV_FORMAT': 'YES',
      });

      final response = await http.get(url).timeout(const Duration(seconds: 15));
      if (response.statusCode == 200) {
        final data = json.decode(response.body);
        final resultStr = data['result'] as String?;
        if (resultStr != null) {
          return _extractCSVEphemeris(name, resultStr);
        }
      }
    } catch (_) {}
    return null;
  }

  Future<List<PlanetData>> _fetchVisiblePlanets() async {
    List<PlanetData> results = [];
    try {
      // Fetch all planets regardless of horizon, include RA/Dec
      final url = Uri.parse('https://api.visibleplanets.dev/v3?latitude=0&longitude=0&aboveHorizon=false&showCoords=true');
      final response = await http.get(url).timeout(const Duration(seconds: 10));
      if (response.statusCode == 200) {
        final data = json.decode(response.body);
        final planetList = data['data'] as List;
        for (var p in planetList) {
          final name = p['name'];
          if (_planetIds.containsKey(name)) {
            results.add(PlanetData(
              name: name,
              rightAscension: p['rightAscension']['raw'] as double,
              declination: p['declination']['raw'] as double,
              distanceToSun: 0.0, // Visible Planets doesn't provide AU distances
              distanceToEarth: 0.0,
            ));
          }
        }
      }
    } catch (_) {}
    return results;
  }

  PlanetData? _extractCSVEphemeris(String name, String result) {
    try {
      final startIndex = result.indexOf(r'$$SOE');
      final endIndex = result.indexOf(r'$$EOE');
      if (startIndex == -1 || endIndex == -1) return null;

      // Clean the block of internal newlines
      String block = result.substring(startIndex + 5, endIndex);
      block = block.replaceAll('\n', '').replaceAll('\r', '').trim();

      // Split by comma
      final parts = block.split(',');
      if (parts.length < 8) return null;

      // CSV format for QUANTITIES='1,19,20':
      // 0: Date, 1: (empty), 2: (empty), 3: RA (HH MM SS.ss), 4: Dec (DD MM SS.s), 5: r (Sun), 6: rdot, 7: delta (Earth), 8: deldot
      
      // Parse RA
      final raStr = parts[3].trim();
      final raParts = raStr.split(RegExp(r'\s+'));
      double ra = double.parse(raParts[0]) + 
                   double.parse(raParts[1]) / 60.0 + 
                   double.parse(raParts[2]) / 3600.0;

      // Parse Dec
      final decStr = parts[4].trim();
      final decParts = decStr.split(RegExp(r'\s+'));
      double decDeg = double.parse(decParts[0].replaceAll('+', ''));
      bool isNegative = decParts[0].contains('-');
      double dec = decDeg.abs() + 
                    double.parse(decParts[1]) / 60.0 + 
                    double.parse(decParts[2]) / 3600.0;
      if (isNegative) dec = -dec;

      return PlanetData(
        name: name,
        rightAscension: ra,
        declination: dec,
        distanceToSun: double.parse(parts[5].trim()),
        distanceToEarth: double.parse(parts[7].trim()),
      );
    } catch (_) {
      return null;
    }
  }
}
