  $ (cd ../../../../default && demos/parserTestPack.exe)
  
  -_-_-_-_-_-_-_-_-_-_- Console writeline -_-_-_-_-_-_-_-_-_-_-
  
  Class
  ([Public], Name ("Program"), [],
   [([Public; Static],
     Method
     (TVoid, Name ("Main"), None, [],
      Some (StatementBlock ([VariableDecl
                             (None, TInt,
                              [(Name ("a"), Some (Value (VInt (23))))]);
                             Print (Value (VInt (23)));
                             VariableDecl
                             (None, TInt,
                              [(Name ("a"), Some (Value (VInt (5))))]);
                             Print (Identifier ("a"))]))))])
  -_-_-_-_-_-_-_-_-_-_- Console writeline -_-_-_-_-_-_-_-_-_-_-
  
  Class
  ([Public], Name ("Program"), [],
   [([Public; Static],
     Method
     (TVoid, Name ("Main"), None, [],
      Some (StatementBlock ([Print (Value (VString ("aaaaa")));
                             Print (Value (VInt (42)))]))))])
  -_-_-_-_-_-_-_-_-_-_- Assing to method -_-_-_-_-_-_-_-_-_-_-
  
  Class
  ([Public], Name ("Program"), [],
   [([Public; Static],
     Method
     (TVoid, Name ("Main"), None, [],
      Some (StatementBlock ([Expression (Assign
                                         (AccessByPoint
                                          (Identifier ("a"),
                                           CallMethod (Identifier ("F"), [])),
                                          Value (VInt (42))))]))))])
  -_-_-_-_-_-_-_-_-_-_- Cast difficult expression -_-_-_-_-_-_-_-_-_-_-
  
  Class
  ([Public], Name ("Program"), [],
   [([Public; Static],
     Method
     (TVoid, Name ("Main"), None, [],
      Some (StatementBlock ([VariableDecl
                             (None, TRef ("var"),
                              [(Name ("a"),
                                Some (ClassCreation (Name ("A"), [])))]);
                             Print (Value (VString ("######################################")));
                             Print (AccessByPoint
                                    (Cast (TRef ("A"), Identifier ("a")),
                                     CallMethod (Identifier ("F"), [])))]))))])
  -_-_-_-_-_-_-_-_-_-_- Cast -_-_-_-_-_-_-_-_-_-_-
  
  Class
  ([Public], Name ("Program"), [],
   [([Public; Static],
     Method
     (TVoid, Name ("Main"), None, [],
      Some (StatementBlock ([VariableDecl
                             (None, TInt,
                              [(Name ("a"),
                                Some (Cast (TInt, Value (VInt (23)))))]);
                             VariableDecl
                             (None, TInt,
                              [(Name ("b"),
                                Some (Cast (TInt, Identifier ("a"))))]);
                             VariableDecl
                             (None, TBool,
                              [(Name ("c"),
                                Some (Cast (TBool, Identifier ("b"))))]);
                             Expression (Assign
                                         (Identifier ("b"),
                                          Cast (TInt, Identifier ("c"))))]))))])
  -_-_-_-_-_-_-_-_-_-_- Cast to class -_-_-_-_-_-_-_-_-_-_-
  
  Class
  ([Public], Name ("Program"), [],
   [([Public; Static],
     Method
     (TVoid, Name ("Main"), None, [],
      Some (StatementBlock ([VariableDecl
                             (None, TRef ("SomeClass"),
                              [(Name ("a"),
                                Some (ClassCreation (Name ("SomeClass"), [])))]);
                             Print (AccessByPoint
                                    (Identifier ("a"),
                                     CallMethod (Identifier ("F"), [])));
                             Expression (Assign
                                         (Identifier ("a"),
                                          Cast
                                          (TRef ("SomeClass"),
                                           Identifier ("a"))));
                             VariableDecl
                             (None, TRef ("SomeClass"),
                              [(Name ("b"),
                                Some (Cast
                                      (TRef ("SomeClass"), Identifier ("a"))))]);
                             VariableDecl
                             (None, TInt,
                              [(Name ("c"),
                                Some (Cast
                                      (TInt,
                                       AccessByPoint
                                       (Identifier ("b"),
                                        CallMethod (Identifier ("F"), [])))))]);
                             Print (AccessByPoint
                                    (Identifier ("a"),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Identifier ("b"),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Identifier ("C"),
                                     CallMethod (Identifier ("F"), [])))]))))])
  Class
  ([Public], Name ("SomeClass"), [],
   [([Public],
     Method
     (TInt, Name ("F"), None, [],
      Some (StatementBlock ([Return (Some (Value (VInt (123))))]))))])
  -_-_-_-_-_-_-_-_-_-_- Interface -_-_-_-_-_-_-_-_-_-_-
  
  Class
  ([Public], Name ("Program"), [],
   [([Public; Static],
     Method
     (TVoid, Name ("Main"), None, [],
      Some (StatementBlock ([VariableDecl
                             (None, TRef ("IInterface"),
                              [(Name ("a"),
                                Some (ClassCreation (Name ("SomeClass"), [])))])]))))])
  Interface (Public, Name ("IInterface"), [], [])
  Class ([Public], Name ("SomeClass"), [Name ("IInterface")], [])
  -_-_-_-_-_-_-_-_-_-_- Many parents -_-_-_-_-_-_-_-_-_-_-
  
  Class
  ([Public], Name ("Program"), [],
   [([Public; Static],
     Method (TVoid, Name ("Main"), None, [], Some (StatementBlock ([]))))])
  Interface (Public, Name ("IInterface"), [], [])
  Class ([Public], Name ("A"), [], [])
  Class ([Public], Name ("B"), [Name ("IInterface"); Name ("A")], [])
  Class ([Public], Name ("C"), [Name ("IInterface"); Name ("A")], [])
 
