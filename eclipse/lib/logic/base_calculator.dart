import 'eclipse_event.dart';

abstract class BaseCalculator {
  String get name;
  List<EclipseEvent> calculateEclipses(int startYear, {int count = 10});
}
