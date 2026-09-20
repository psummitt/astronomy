import 'package:flutter/material.dart';
import '../models/star_data.dart';
import '../services/star_service.dart';
import '../services/astronomy_service.dart';
import '../services/location_service.dart';
import 'how_to_use_page.dart';

class HomePage extends StatefulWidget {
  const HomePage({super.key});

  @override
  State<HomePage> createState() => _HomePageState();
}

class _HomePageState extends State<HomePage> {
  final TextEditingController _controller = TextEditingController();
  StarData? _starData;
  bool _isLoading = false;
  String? _errorMessage;

  Future<void> _searchStar() async {
    final name = _controller.text.trim();
    if (name.isEmpty) return;

    setState(() {
      _isLoading = true;
      _errorMessage = null;
      _starData = null;
    });

    try {
      // 1. Fetch Star Data (Fixed Coordinates)
      final star = await StarService.fetchStarData(name);
      if (star == null) {
        setState(() {
          _errorMessage = 'Star not found. Please check the name.';
          _isLoading = false;
        });
        return;
      }

      // 2. Get Current Location
      final position = await LocationService.getCurrentLocation();
      if (position == null) {
        setState(() {
          _starData = star; // Still show fixed data
          _errorMessage = 'Location access denied. Altitude and Bearing cannot be calculated.';
          _isLoading = false;
        });
        return;
      }

      // 3. Calculate Alt/Az
      final updatedStar = AstronomyService.calculateAltAz(
        star,
        position.latitude,
        position.longitude,
      );

      setState(() {
        _starData = updatedStar;
        _isLoading = false;
      });
    } catch (e) {
      setState(() {
        _errorMessage = 'An error occurred during search.';
        _isLoading = false;
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('StarSearch'),
        actions: [
          IconButton(
            icon: const Icon(Icons.help_outline),
            tooltip: 'How to use',
            onPressed: () {
              Navigator.push(
                context,
                MaterialPageRoute(builder: (context) => const HowToUsePage()),
              );
            },
          ),
        ],
      ),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          children: [
            Semantics(
              label: 'Enter star name',
              child: TextField(
                controller: _controller,
                decoration: InputDecoration(
                  labelText: 'Star Name',
                  hintText: 'e.g., Sirius',
                  suffixIcon: IconButton(
                    icon: const Icon(Icons.search),
                    onPressed: _searchStar,
                  ),
                  border: const OutlineInputBorder(),
                ),
                onSubmitted: (_) => _searchStar(),
              ),
            ),
            const SizedBox(height: 20),
            if (_isLoading)
              const CircularProgressIndicator()
            else if (_errorMessage != null)
              Text(
                _errorMessage!,
                style: const TextStyle(color: Colors.red),
              ),
            if (_starData != null) _buildResultCard(),
          ],
        ),
      ),
    );
  }

  Widget _buildResultCard() {
    return Card(
      elevation: 4,
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Semantics(
              header: true,
              child: Text(
                _starData!.name.toUpperCase(),
                style: Theme.of(context).textTheme.headlineMedium,
              ),
            ),
            const Divider(),
            _buildInfoRow(Icons.brightness_7, 'Brightness (Mag)', 
                _starData!.magnitude?.toStringAsFixed(2) ?? 'N/A'),
            _buildInfoRow(Icons.location_on, 'RA', 
                '${_starData!.ra.toStringAsFixed(4)}°'),
            _buildInfoRow(Icons.location_on, 'Dec', 
                '${_starData!.dec.toStringAsFixed(4)}°'),
            if (_starData!.altitude != null)
              _buildInfoRow(Icons.height, 'Altitude', 
                  '${_starData!.altitude!.toStringAsFixed(2)}°'),
            if (_starData!.azimuth != null)
              _buildInfoRow(Icons.compass_calibration, 'Bearing (Azimuth)', 
                  '${_starData!.azimuth!.toStringAsFixed(2)}°'),
          ],
        ),
      ),
    );
  }

  Widget _buildInfoRow(IconData icon, String label, String value) {
    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 4.0),
      child: Row(
        children: [
          Icon(icon, size: 20, color: Colors.blueGrey),
          const SizedBox(width: 8),
          Expanded(
            child: Semantics(
              label: '$label: $value',
              child: Text('$label:'),
            ),
          ),
          Text(
            value,
            style: const TextStyle(fontWeight: FontWeight.bold),
          ),
        ],
      ),
    );
  }
}
