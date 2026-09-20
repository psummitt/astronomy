import 'package:flutter/material.dart';
import '../models/orbit_elements.dart';
import '../models/solar_coordinates.dart';
import '../models/calculation_result.dart';
import 'orbit_engine.dart';

class OrbitProvider with ChangeNotifier {
  OrbitElements elements = OrbitElements();
  List<SolarCoordinates> solarCoordinatesList = List.generate(10, (_) => SolarCoordinates());
  List<CalculationResult> results = [];
  Precision precision = Precision.double;

  void setPrecision(Precision p) {
    precision = p;
    notifyListeners();
  }

  void updateSolarCoordinate(int index, SolarCoordinates coords) {
    solarCoordinatesList[index] = coords;
    notifyListeners();
  }

  void calculate() {
    final engine = OrbitEngine(precision: precision);
    results = engine.calculate(elements, solarCoordinatesList);
    notifyListeners();
  }
}
