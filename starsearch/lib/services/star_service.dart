import 'package:http/http.dart' as http;
import 'package:xml/xml.dart';
import '../models/star_data.dart';

class StarService {
  static Future<StarData?> fetchStarData(String starName) async {
    // Sesame URL with XML output (-ox) and fluxes (-oF)
    final url = Uri.parse('https://cdsweb.u-strasbg.fr/cgi-bin/nph-sesame/-oxF/SN?${Uri.encodeComponent(starName)}');
    
    try {
      final response = await http.get(url);
      if (response.statusCode == 200) {
        final document = XmlDocument.parse(response.body);
        final targetElements = document.findAllElements('Target');
        if (targetElements.isEmpty) return null;
        final target = targetElements.first;

        final resolverElements = target.findAllElements('Resolver');
        if (resolverElements.isEmpty) return null;
        final resolver = resolverElements.first;

        final raElements = resolver.findElements('jradeg');
        final decElements = resolver.findElements('jdedeg');
        
        if (raElements.isEmpty || decElements.isEmpty) return null;

        final ra = double.parse(raElements.first.innerText);
        final dec = double.parse(decElements.first.innerText);

        // Try to find V magnitude
        double? mag;
        final magElements = resolver.findElements('mag');
        for (var element in magElements) {
          if (element.getAttribute('band') == 'V') {
            final vElements = element.findElements('v');
            if (vElements.isNotEmpty) {
              mag = double.tryParse(vElements.first.innerText);
            }
            break;
          }
        }

        // If no V band, just take the first one available
        if (mag == null && magElements.isNotEmpty) {
           final vElements = magElements.first.findElements('v');
           if (vElements.isNotEmpty) {
             mag = double.tryParse(vElements.first.innerText);
           }
        }

        return StarData.fromSesame(starName, ra, dec, mag);
      }
    } catch (e) {
      print('Error fetching star data: $e');
    }
    return null;
  }
}
