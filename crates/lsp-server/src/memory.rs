use std::collections::{HashMap, HashSet};
use std::mem;

/// Store a copy of the open documents in memory and tracks documents with unprocessed changes.
pub struct Memory {
    docs: HashMap<String, Document>,
    changes: HashSet<String>,
}

impl Memory {
    /// Creates a new instance.
    pub fn new() -> Self {
        Self {
            docs: HashMap::new(),
            changes: HashSet::new(),
        }
    }

    /// Adds document.
    pub fn insert(&mut self, id: String, content: String, version: i32) {
        self.docs.insert(id.clone(), Document { content, version });
        self.changes.insert(id);
    }

    /// Retrieves document.
    pub fn get(&self, id: &str) -> Option<&Document> {
        self.docs.get(id)
    }

    /// Updates document.
    pub fn update(&mut self, id: &str, content: String, version: i32) -> bool {
        match self.docs.get_mut(id) {
            Some(doc) => {
                doc.content = content;
                doc.version = version;
                self.changes.insert(id.to_string());
                true
            }
            None => false,
        }
    }

    /// Removes document.
    pub fn remove(&mut self, id: &str) -> Option<Document> {
        self.docs.remove(id).map(|doc| {
            self.changes.insert(id.to_string());
            doc
        })
    }

    /// Checks if any documents have unprocessed changes.
    pub fn has_changes(&self) -> bool {
        !self.changes.is_empty()
    }

    /// Retrieves the document identifiers for documents with unprocessed changes and clears the change tracker.
    pub fn take_changes(&mut self) -> HashSet<String> {
        mem::take(&mut self.changes)
    }
}

/// Represents a text document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Document {
    /// The contents of the text documents.
    pub content: String,
    /// The version of the text document as defined by the LSP client.
    pub version: i32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn memory_works() {
        // Create memory instance.
        let mut memory = Memory::new();

        // Verifies has changes.
        assert!(!memory.has_changes());

        // Add documents.
        memory.insert("1".to_string(), "A".to_string(), 0);
        memory.insert("2".to_string(), "B".to_string(), 0);
        memory.insert("3".to_string(), "C".to_string(), 0);

        // Verifies has and take changes.
        assert!(memory.has_changes());
        assert_eq!(
            memory.take_changes(),
            HashSet::from(["1".to_string(), "2".to_string(), "3".to_string()])
        );
        assert!(!memory.has_changes());

        // Retrieve document.
        assert_eq!(
            memory.get("1"),
            Some(&Document {
                content: "A".to_string(),
                version: 0
            })
        );
        assert_eq!(memory.get("0"), None);

        // Update document.
        memory.update("1", "A1".to_string(), 1);
        assert_eq!(
            memory.get("1"),
            Some(&Document {
                content: "A1".to_string(),
                version: 1
            })
        );

        // Verifies has and take changes.
        assert!(memory.has_changes());
        assert_eq!(memory.take_changes(), HashSet::from(["1".to_string()]));
        assert!(!memory.has_changes());

        // Remove document.
        memory.remove("1");
        assert_eq!(memory.get("1"), None);

        // Verifies has and take changes.
        assert!(memory.has_changes());
        assert_eq!(memory.take_changes(), HashSet::from(["1".to_string()]));
        assert!(!memory.has_changes());
    }
}
