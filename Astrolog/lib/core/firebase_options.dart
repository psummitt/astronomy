// ignore_for_file: type=lint
import 'package:firebase_core/firebase_core.dart' show FirebaseOptions;
import 'package:flutter/foundation.dart'
    show defaultTargetPlatform, kIsWeb, TargetPlatform;

/// Default [FirebaseOptions] for AstroLog.
class DefaultFirebaseOptions {
  static FirebaseOptions? get currentPlatform {
    if (kIsWeb) {
      return web;
    }
    switch (defaultTargetPlatform) {
      case TargetPlatform.android:
        return android;
      case TargetPlatform.windows:
        return windows;
      case TargetPlatform.linux:
        // Return web configuration as fallback or null if Firebase is optional
        return web;
      default:
        return null;
    }
  }

  static const FirebaseOptions web = FirebaseOptions(
    apiKey: 'AIzaSyCrDqenB8Xx9hyNn4NL9_QOxPSMI8sFF6I',
    appId: '1:974454311997:web:0f03fb900be72c75e0ae54',
    messagingSenderId: '974454311997',
    projectId: 'astronomy-logbook',
    authDomain: 'astronomy-logbook.firebaseapp.com',
    storageBucket: 'astronomy-logbook.firebasestorage.app',
    measurementId: 'G-TKBHNVF4C2',
  );

  static const FirebaseOptions android = FirebaseOptions(
    apiKey: 'AIzaSyDkD1MhSxxpUu28ZeG4e0Xs9ySJMlRL44k',
    appId: '1:974454311997:android:c4b8e0da83fdea1de0ae54',
    messagingSenderId: '974454311997',
    projectId: 'astronomy-logbook',
    storageBucket: 'astronomy-logbook.firebasestorage.app',
  );

  static const FirebaseOptions windows = FirebaseOptions(
    apiKey: 'AIzaSyCrDqenB8Xx9hyNn4NL9_QOxPSMI8sFF6I',
    appId: '1:974454311997:web:6ee596e9ae9fd07ee0ae54',
    messagingSenderId: '974454311997',
    projectId: 'astronomy-logbook',
    authDomain: 'astronomy-logbook.firebaseapp.com',
    storageBucket: 'astronomy-logbook.firebasestorage.app',
    measurementId: 'G-SZRFV9RK70',
  );
}
