import 'package:http/http.dart' as http;
import 'package:xml/xml.dart';

class StarData {
  final String name;
  final double ra;  // Hours (0-24) or degrees depending on source
  final double dec; // Degrees
  final double? magnitude;

  StarData({
    required this.name,
    required this.ra,
    required this.dec,
    this.magnitude,
  });

  /// Formatted RA string (hours, minutes, seconds)
  String get raFormatted {
    double raHours = ra > 24 ? ra / 15.0 : ra;
    int h = raHours.floor();
    double mFull = (raHours - h) * 60.0;
    int m = mFull.floor();
    int s = ((mFull - m) * 60.0).round();
    if (s >= 60) {
      s = 0;
      m += 1;
    }
    if (m >= 60) {
      m = 0;
      h = (h + 1) % 24;
    }
    return '${h.toString().padLeft(2, '0')}h ${m.toString().padLeft(2, '0')}m ${s.toString().padLeft(2, '0')}s';
  }

  /// Formatted Declination string
  String get decFormatted {
    double absDec = dec.abs();
    int d = absDec.floor();
    double mFull = (absDec - d) * 60.0;
    int m = mFull.floor();
    int s = ((mFull - m) * 60.0).round();
    if (s >= 60) {
      s = 0;
      m += 1;
    }
    String sign = dec >= 0 ? '+' : '-';
    return '$sign${d.toString().padLeft(2, '0')}° ${m.toString().padLeft(2, '0')}\' ${s.toString().padLeft(2, '0')}"';
  }
}

class StarService {
  /// Fetches star data from CDS Strasbourg Sesame service.
  static Future<StarData?> fetchStarData(String starName) async {
    final url = Uri.parse('https://cdsweb.u-strasbg.fr/cgi-bin/nph-sesame/-oxF/SN?${Uri.encodeComponent(starName.trim())}');
    
    try {
      final response = await http.get(url).timeout(const Duration(seconds: 5));
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

        final raDeg = double.parse(raElements.first.innerText);
        final decDeg = double.parse(decElements.first.innerText);
        final raHours = raDeg / 15.0;

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

        if (mag == null && magElements.isNotEmpty) {
           final vElements = magElements.first.findElements('v');
           if (vElements.isNotEmpty) {
             mag = double.tryParse(vElements.first.innerText);
           }
        }

        return StarData(
          name: starName,
          ra: raHours,
          dec: decDeg,
          magnitude: mag,
        );
      }
    } catch (e) {
      // Fallback or offline catalog search could be placed here
    }
    return null;
  }
}
