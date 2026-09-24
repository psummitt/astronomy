import 'package:cloud_firestore/cloud_firestore.dart';
import 'package:flutter/foundation.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../../../core/providers/auth_providers.dart';
import '../../../core/providers/firebase_providers.dart';
import '../domain/observation_log.dart';

class ObservationRepository {
  final FirebaseFirestore _firestore;

  ObservationRepository(this._firestore);

  Stream<List<ObservationLog>> getObservationsStream(String userId) {
    if (userId.isEmpty) {
      return Stream.value([]);
    }

    try {
      return _firestore
          .collection('observations')
          .where('userId', isEqualTo: userId)
          .snapshots()
          .map((snapshot) {
        final docs = snapshot.docs.map((doc) {
          return ObservationLog.fromJson(doc.data(), doc.id);
        }).toList();

        // Sort locally in memory (newest observation date first)
        docs.sort((a, b) => b.observationDate.compareTo(a.observationDate));
        return docs;
      });
    } catch (e) {
      debugPrint('Error creating observations stream: $e');
      return Stream.value([]);
    }
  }

  Future<void> createObservation(ObservationLog observation) async {
    final docRef = _firestore.collection('observations').doc();
    final data = observation.toJson();
    data['id'] = docRef.id;
    await docRef.set(data);
  }

  Future<void> updateObservation(ObservationLog observation) async {
    await _firestore
        .collection('observations')
        .doc(observation.id)
        .update(observation.toJson());
  }

  Future<void> deleteObservation(String id) async {
    await _firestore.collection('observations').doc(id).delete();
  }
}

final observationRepositoryProvider = Provider<ObservationRepository>((ref) {
  return ObservationRepository(ref.watch(firestoreProvider));
});

final observationsStreamProvider =
    StreamProvider<List<ObservationLog>>((ref) {
  final user = ref.watch(currentUserProvider);
  if (user == null) {
    return Stream.value([]);
  }
  return ref
      .watch(observationRepositoryProvider)
      .getObservationsStream(user.uid);
});
