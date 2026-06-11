import 'dart:convert';

class Holiday {
  final String id;
  final String name;
  final int month;
  final int day;
  final int? year; // null if it repeats every year
  final bool isCustom;
  final bool isEnabled;

  Holiday({
    required this.id,
    required this.name,
    required this.month,
    required this.day,
    this.year,
    this.isCustom = false,
    this.isEnabled = true,
  });

  Holiday copyWith({
    String? id,
    String? name,
    int? month,
    int? day,
    int? year,
    bool? isCustom,
    bool? isEnabled,
  }) {
    return Holiday(
      id: id ?? this.id,
      name: name ?? this.name,
      month: month ?? this.month,
      day: day ?? this.day,
      year: year ?? this.year,
      isCustom: isCustom ?? this.isCustom,
      isEnabled: isEnabled ?? this.isEnabled,
    );
  }

  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'name': name,
      'month': month,
      'day': day,
      'year': year,
      'isCustom': isCustom,
      'isEnabled': isEnabled,
    };
  }

  factory Holiday.fromMap(Map<String, dynamic> map) {
    return Holiday(
      id: map['id'] ?? '',
      name: map['name'] ?? '',
      month: map['month'] ?? 1,
      day: map['day'] ?? 1,
      year: map['year'],
      isCustom: map['isCustom'] ?? false,
      isEnabled: map['isEnabled'] ?? true,
    );
  }

  String toJson() => json.encode(toMap());

  factory Holiday.fromJson(String source) => Holiday.fromMap(json.decode(source));

  @override
  String toString() {
    return 'Holiday(id: $id, name: $name, date: $month/$day${year != null ? "/$year" : " (yearly)"}, isCustom: $isCustom, isEnabled: $isEnabled)';
  }
}
