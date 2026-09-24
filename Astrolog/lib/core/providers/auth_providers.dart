import 'package:firebase_auth/firebase_auth.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'firebase_providers.dart';

final authStateChangesProvider = StreamProvider<User?>((ref) {
  try {
    return ref.watch(firebaseAuthProvider).authStateChanges();
  } catch (_) {
    return Stream.value(null);
  }
});

final currentUserProvider = Provider<User?>((ref) {
  return ref.watch(authStateChangesProvider).asData?.value;
});
