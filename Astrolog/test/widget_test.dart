import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:astrolog/app/app.dart';

void main() {
  testWidgets('AstroLog app basic smoke test', (WidgetTester tester) async {
    await tester.pumpWidget(
      const ProviderScope(
        child: AstroLogApp(),
      ),
    );
    expect(find.byType(AstroLogApp), findsOneWidget);
  });
}
