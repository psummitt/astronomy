import 'package:flutter/material.dart';
import 'settings_model.dart';
import 'main.dart';
import 'solar_calculator.dart';

class ClassicModeScreen extends StatefulWidget {
  final SunfinderSettings settings;
  const ClassicModeScreen({super.key, required this.settings});

  @override
  State<ClassicModeScreen> createState() => _ClassicModeScreenState();
}

enum ClassicState {
  menu,
  firstDate,
  lastDate,
  firstHour,
  lastHour,
  increment,
  calculating,
}

class _ClassicModeScreenState extends State<ClassicModeScreen> {
  final List<String> _output = [];
  final TextEditingController _inputController = TextEditingController();
  final ScrollController _scrollController = ScrollController();
  
  ClassicState _state = ClassicState.menu;
  int _programChoice = 0;
  int _startMonth = 0, _startDay = 0;
  int _endMonth = 0, _endDay = 0;
  double _startHour = 0, _endHour = 0, _increment = 1;

  @override
  void initState() {
    super.initState();
    _printMenu();
  }

  void _printMenu() {
    setState(() {
      _output.add("WHAT PROGRAM DO YOU WANT TO RUN?");
      _output.add("1=WHERE'S THE SUN");
      _output.add("2=SUNRISE-SUNSET");
      _output.add("3=QUIT");
      _output.add("?");
    });
    _scrollToBottom();
  }

  void _handleInput(String input) {
    setState(() {
      _output.add("> $input");
    });
    
    switch (_state) {
      case ClassicState.menu:
        _programChoice = int.tryParse(input) ?? 0;
        if (_programChoice == 3) {
           final newSettings = widget.settings;
           newSettings.isModernMode = true;
           SunfinderApp.of(context).updateSettings(newSettings);
        } else if (_programChoice == 1 || _programChoice == 2) {
          _state = ClassicState.firstDate;
          _output.add("FIRST DATE (MONTH, DAY)?");
        } else {
          _output.add("INVALID CHOICE.");
          _printMenu();
        }
        break;
      case ClassicState.firstDate:
        final parts = input.split(RegExp(r'[, ]+'));
        if (parts.length >= 2) {
          _startMonth = int.tryParse(parts[0]) ?? 1;
          _startDay = int.tryParse(parts[1]) ?? 1;
          _state = ClassicState.lastDate;
          _output.add("LAST DATE (MONTH, DAY)?");
        } else {
          _output.add("REENTER (M, D).");
        }
        break;
      case ClassicState.lastDate:
        final parts = input.split(RegExp(r'[, ]+'));
        if (parts.length >= 2) {
          _endMonth = int.tryParse(parts[0]) ?? 1;
          _endDay = int.tryParse(parts[1]) ?? 1;
          if (_programChoice == 1) {
            _state = ClassicState.firstHour;
            _output.add("FIRST HOUR (0-24)?");
          } else {
            _state = ClassicState.increment;
            _output.add("INCREMENT IN DAYS?");
          }
        } else {
          _output.add("REENTER (M, D).");
        }
        break;
      case ClassicState.firstHour:
        _startHour = double.tryParse(input) ?? 0;
        _state = ClassicState.lastHour;
        _output.add("LAST HOUR (0-24)?");
        break;
      case ClassicState.lastHour:
        _endHour = double.tryParse(input) ?? 0;
        _state = ClassicState.increment;
        _output.add("INCREMENT IN HOURS?");
        break;
      case ClassicState.increment:
        _increment = double.tryParse(input) ?? 1;
        _runCalculation();
        break;
      default:
        break;
    }
    _inputController.clear();
    _scrollToBottom();
  }

  void _runCalculation() {
    _output.add("DATE\tHOUR\tAZIMUTH\tALTITUDE");
    final startDoy = SolarCalculator.getDayOfYear(_startMonth, _startDay, widget.settings.isLeapYear);
    final endDoy = SolarCalculator.getDayOfYear(_endMonth, _endDay, widget.settings.isLeapYear);

    if (_programChoice == 1) {
      final res = SolarCalculator.calculatePosition(
        latitude: widget.settings.latitude,
        longitude: widget.settings.longitude,
        standardMeridian: widget.settings.standardMeridian,
        magneticVariation: widget.settings.magneticVariation,
        isLeapYear: widget.settings.isLeapYear,
        startDoy: startDoy,
        endDoy: endDoy,
        startHour: _startHour,
        endHour: _endHour,
        hourIncrement: _increment,
      );
      for (var r in res) {
        _output.add("${r.dateString}\t${r.hour.toStringAsFixed(1)}\t${r.azimuth}\t${r.altitude}");
      }
    } else {
      final res = SolarCalculator.calculateSunriseSunset(
        latitude: widget.settings.latitude,
        longitude: widget.settings.longitude,
        standardMeridian: widget.settings.standardMeridian,
        magneticVariation: widget.settings.magneticVariation,
        isLeapYear: widget.settings.isLeapYear,
        startDoy: startDoy,
        endDoy: endDoy,
        dayIncrement: _increment.toInt(),
      );
      for (var r in res) {
        _output.add("${r.dateString}\tSU:${r.sunrise}@${r.sunriseAzimuth}\tSD:${r.sunset}@${r.sunsetAzimuth}\tNOON:${r.noonAltitude}");
      }
    }
    _output.add("CALCULATION COMPLETE.");
    _state = ClassicState.menu;
    _printMenu();
  }

  void _scrollToBottom() {
    Future.delayed(const Duration(milliseconds: 100), () {
      if (_scrollController.hasClients) {
        _scrollController.animateTo(
          _scrollController.position.maxScrollExtent,
          duration: const Duration(milliseconds: 300),
          curve: Curves.easeOut,
        );
      }
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.black,
      appBar: AppBar(
        title: const Text("TRS-80 EMULATOR (SUNFINDER)", style: TextStyle(fontFamily: 'Courier')),
        backgroundColor: Colors.grey[900],
        foregroundColor: Colors.green,
      ),
      body: Column(
        children: [
          Expanded(
            child: ListView.builder(
              controller: _scrollController,
              padding: const EdgeInsets.all(16),
              itemCount: _output.length,
              itemBuilder: (context, index) => Text(
                _output[index],
                style: const TextStyle(color: Colors.green, fontFamily: 'Courier', fontSize: 16),
              ),
            ),
          ),
          Container(
            padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
            color: Colors.grey[900],
            child: Row(
              children: [
                const Text(">", style: TextStyle(color: Colors.green, fontSize: 18)),
                const SizedBox(width: 8),
                Expanded(
                  child: TextField(
                    controller: _inputController,
                    autofocus: true,
                    style: const TextStyle(color: Colors.green, fontFamily: 'Courier'),
                    decoration: const InputDecoration(border: InputBorder.none),
                    onSubmitted: _handleInput,
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }
}
