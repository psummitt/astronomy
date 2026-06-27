import 'package:flutter/material.dart';
import 'dart:math' as math;

void main() {
  runApp(const EpochApp());
}

class EpochApp extends StatelessWidget {
  const EpochApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Epoch I - Precession Calculator',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.indigo),
        useMaterial3: true,
      ),
      home: const PrecessionPage(),
    );
  }
}

class PrecessionPage extends StatefulWidget {
  const PrecessionPage({super.key});

  @override
  State<PrecessionPage> createState() => _PrecessionPageState();
}

class _PrecessionPageState extends State<PrecessionPage> {
  final _formKey = GlobalKey<FormState>();
  
  final _y1Controller = TextEditingController(text: "1950");
  final _y2Controller = TextEditingController(text: "2000");
  
  final _raHController = TextEditingController(text: "0");
  final _raMController = TextEditingController(text: "0");
  final _raSController = TextEditingController(text: "0");
  
  final _decDController = TextEditingController(text: "0");
  final _decMController = TextEditingController(text: "0");
  final _decSController = TextEditingController(text: "0");

  String _result = "";

  void _calculate() {
    if (!_formKey.currentState!.validate()) return;

    double y1 = double.tryParse(_y1Controller.text) ?? 1950;
    double y2 = double.tryParse(_y2Controller.text) ?? 2000;

    double raH = double.tryParse(_raHController.text) ?? 0;
    double raM = double.tryParse(_raMController.text) ?? 0;
    double raS = double.tryParse(_raSController.text) ?? 0;

    double decD = double.tryParse(_decDController.text) ?? 0;
    double decM = double.tryParse(_decMController.text) ?? 0;
    double decS = double.tryParse(_decSController.text) ?? 0;

    // Convert inputs to decimal hours and decimal degrees
    double r1Hours = raH + raM / 60.0 + raS / 3600.0;
    double d1Deg;
    if (decD < 0) {
      d1Deg = decD - decM / 60.0 - decS / 3600.0;
    } else {
      d1Deg = decD + decM / 60.0 + decS / 3600.0;
    }

    // Precession Logic from Epoch.bas
    double toRad(double deg) => deg * math.pi / 180.0;
    
    // T2 = ((Y2 + Y1 ) / 2 - 1900) / 100
    double t2 = ((y2 + y1) / 2.0 - 1900.0) / 100.0;
    
    // Based on Epoch.bas lines 310-330
    // Standard precession formulas:
    // m = 3.07234 + 0.00186 * T
    // n = 20.0468 - 0.0085 * T
    // Note: The original BASIC had '+' instead of '*', likely a typo.
    double x = 3.07234 + (0.00186 * t2); 
    double y = 20.0468 - (0.0085 * t2);
    double z = y / 15.0;
    double t = y2 - y1;
    
    double r1Deg = r1Hours * 15.0;
    
    // 590 W = .0042 * T * (X + (Z * SIN ( FN RAD(R1 )) * TAN( FN Rad(D1))))
    double w = 0.0042 * t * (x + (z * math.sin(toRad(r1Deg)) * math.tan(toRad(d1Deg))));
    double r2Deg = r1Deg + w;
    
    // 610 D2 = D1 + .00028 * T * Y * COS ( FN RAD(R1 ))
    double d2Deg = d1Deg + 0.00028 * t * y * math.cos(toRad(r1Deg));
    
    double r2Hours = r2Deg / 15.0;
    while (r2Hours >= 24) { r2Hours -= 24; }
    while (r2Hours < 0) { r2Hours += 24; }
    
    // Format RA for display
    int outRaH = r2Hours.floor();
    double raMinTotal = (r2Hours - outRaH) * 60.0;
    int outRaM = raMinTotal.floor();
    double outRaS = (raMinTotal - outRaM) * 60.0;
    
    // Format Dec for display
    double absD2 = d2Deg.abs();
    int outDecD = d2Deg.truncate();
    double decMinTotal = (absD2 - absD2.floor()) * 60.0;
    int outDecM = decMinTotal.floor();
    double outDecS = (decMinTotal - outDecM) * 60.0;

    setState(() {
      _result = "AT EPOCH $y2\n\n"
          "RIGHT ASCENSION IS ... ${r2Hours.toStringAsFixed(4)} HRS\n"
          "                OR ... $outRaH, $outRaM, ${outRaS.toStringAsFixed(0)}\n"
          "                       HR,MI,SE\n\n"
          "DECLINATION IS ....... ${d2Deg.toStringAsFixed(4)} DEG\n"
          "                OR ...... $outDecD, $outDecM, ${outDecS.toStringAsFixed(0)}\n"
          "                       HR,MI,SE"; // Following the BASIC labels
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('EPOCH I'),
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        actions: [
          IconButton(
            icon: const Icon(Icons.help_outline),
            tooltip: 'Help and Instructions',
            onPressed: () {
              showDialog(
                context: context,
                builder: (context) => AlertDialog(
                  title: const Text("How to Use Epoch I"),
                  content: const SingleChildScrollView(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text("1. Enter the First Epoch (current year of coordinates, e.g., 1950)."),
                        SizedBox(height: 8),
                        Text("2. Enter the Second Epoch (the target year, e.g., 2025)."),
                        SizedBox(height: 8),
                        Text("3. Input R.A. (Hours, Minutes, Seconds) and Dec. (Degrees, Minutes, Seconds) for the First Epoch."),
                        SizedBox(height: 8),
                        Text("4. Tap CALCULATE to see the precessed coordinates for the target year."),
                        SizedBox(height: 16),
                        Text("Based on the original BASIC program by Eric Burgess.", style: TextStyle(fontStyle: FontStyle.italic, fontSize: 12)),
                      ],
                    ),
                  ),
                  actions: [
                    TextButton(
                      onPressed: () => Navigator.pop(context),
                      child: const Text("CLOSE"),
                    ),
                  ],
                ),
              );
            },
          ),
        ],
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _formKey,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              const Card(
                child: Padding(
                  padding: EdgeInsets.all(16.0),
                  child: Column(
                    children: [
                      Text("AN ASTRONOMY PROGRAM", style: TextStyle(fontWeight: FontWeight.bold)),
                      Text("BY ERIC BURGESS F.R.A.S."),
                      SizedBox(height: 8),
                      Text(
                        "THIS PROGRAM COMPUTES RIGHT ASCENSION AND DECLINATION FOR AN EPOCH WHEN GIVEN RA AND DEC FOR ANOTHER EPOCH, REDUCING FOR PRECESSION.",
                        textAlign: TextAlign.center,
                        style: TextStyle(fontSize: 14),
                      ),
                    ],
                  ),
                ),
              ),
              const SizedBox(height: 20),
              Row(
                children: [
                  Expanded(
                    child: TextFormField(
                      controller: _y1Controller,
                      decoration: const InputDecoration(labelText: 'First Epoch (Year)', border: OutlineInputBorder()),
                      keyboardType: TextInputType.number,
                    ),
                  ),
                  const SizedBox(width: 16),
                  Expanded(
                    child: TextFormField(
                      controller: _y2Controller,
                      decoration: const InputDecoration(labelText: 'Second Epoch (Year)', border: OutlineInputBorder()),
                      keyboardType: TextInputType.number,
                    ),
                  ),
                ],
              ),
              const SizedBox(height: 20),
              const Text(
                "R.A. AT FIRST EPOCH",
                style: TextStyle(fontWeight: FontWeight.bold),
              ),
              const SizedBox(height: 8),
              Row(
                children: [
                  Expanded(
                    child: TextFormField(
                      controller: _raHController,
                      decoration: const InputDecoration(labelText: 'Hours', border: OutlineInputBorder()),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    child: TextFormField(
                      controller: _raMController,
                      decoration: const InputDecoration(labelText: 'Minutes', border: OutlineInputBorder()),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    child: TextFormField(
                      controller: _raSController,
                      decoration: const InputDecoration(labelText: 'Seconds', border: OutlineInputBorder()),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                    ),
                  ),
                ],
              ),
              const SizedBox(height: 20),
              const Text(
                "DEC. AT FIRST EPOCH",
                style: TextStyle(fontWeight: FontWeight.bold),
              ),
              const SizedBox(height: 8),
              Row(
                children: [
                  Expanded(
                    child: TextFormField(
                      controller: _decDController,
                      decoration: const InputDecoration(labelText: 'Degrees', border: OutlineInputBorder()),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true, signed: true),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    child: TextFormField(
                      controller: _decMController,
                      decoration: const InputDecoration(labelText: 'Minutes', border: OutlineInputBorder()),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    child: TextFormField(
                      controller: _decSController,
                      decoration: const InputDecoration(labelText: 'Seconds', border: OutlineInputBorder()),
                      keyboardType: const TextInputType.numberWithOptions(decimal: true),
                    ),
                  ),
                ],
              ),
              const SizedBox(height: 30),
              FilledButton.icon(
                onPressed: _calculate,
                icon: const Icon(Icons.calculate),
                label: const Text('CALCULATE PRECESSION'),
                style: FilledButton.styleFrom(padding: const EdgeInsets.symmetric(vertical: 16)),
              ),
              const SizedBox(height: 30),
              if (_result.isNotEmpty)
                Semantics(
                  liveRegion: true,
                  child: Container(
                    padding: const EdgeInsets.all(16),
                    decoration: BoxDecoration(
                      color: Colors.grey[100],
                      border: Border.all(color: Colors.grey[300]!),
                      borderRadius: BorderRadius.circular(8),
                    ),
                    child: Text(
                      _result,
                      style: const TextStyle(fontFamily: 'monospace', fontSize: 16),
                    ),
                  ),
                ),
            ],
          ),
        ),
      ),
    );
  }
}
