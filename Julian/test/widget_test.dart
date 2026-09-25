import 'package:flutter_test/flutter_test.dart';
import 'package:julian_app/main.dart';

void main() {
  testWidgets('Julian app loads', (WidgetTester tester) async {
    await tester.pumpWidget(const JulianApp());

    expect(find.text('Julian Day Calculator'), findsOneWidget);
    expect(find.text('YEAR'), findsWidgets);
  });
}
