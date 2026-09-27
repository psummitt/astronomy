enum EclipseType {
  total,
  partial,
  penumbral,
}

class EclipseEvent {
  final DateTime date;
  final double magnitude;
  final double? penumbralMagnitude;
  final String engineName;
  final String? bestLocation;
  final double? latitude;
  final double? longitude;
  final EclipseType type;

  EclipseEvent({
    required this.date,
    required this.magnitude,
    required this.engineName,
    required this.type,
    this.penumbralMagnitude,
    this.bestLocation,
    this.latitude,
    this.longitude,
  });

  String get typeName {
    switch (type) {
      case EclipseType.total:
        return 'Total Lunar Eclipse';
      case EclipseType.partial:
        return 'Partial Lunar Eclipse';
      case EclipseType.penumbral:
        return 'Penumbral Lunar Eclipse';
    }
  }
}
