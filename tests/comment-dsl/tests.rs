#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn group_type() {
        let addr_bytes = vec![];
        let checksum = 5;

        let addr = Address::new(addr_bytes.clone(), checksum);
        assert_eq!(addr_bytes, addr.address);
        assert_eq!(checksum, addr.checksum);
    }

    // TODO: find a case that triggers this path
    // #[test]
    // fn no_key_group() {

    // }

    #[test]
    fn with_key_group() {
        let minfee_a = 1;
        let minfee_b = 2;

        let addr = ProtocolParamUpdate::new(minfee_a, minfee_b);
        assert_eq!(minfee_a, addr.minfee_a);
        assert_eq!(minfee_b, addr.minfee_b);
    }

    #[test]
    fn group_choice() {
        // just checking these fields exist with the expected name
        let ebb_block = Block::new_ebb_block_wrapper(vec![]);
        match &ebb_block {
            Block::EbbBlockWrapper(wrapper) => { wrapper.ebb_block_cbor.clone(); },
            _ => {}
        };
        let main_block = Block::new_main_block_wrapper(vec![]);
        match &main_block {
            Block::MainBlockWrapper(wrapper) => { wrapper.main_block_cbor.clone(); },
            _ => {}
        };
        assert!(true);
    }

    #[test]
    fn type_choice() {
        // just checking these fields exist with the expected name
        Typechoice::new_case1(Case1::new(vec![]));
        Typechoice::new_case2(Case2::new(vec![]));
        assert!(true);
    }

    #[test]
    fn newtype() {
        let pm = ProtocolMagic::new(5);
        assert_eq!(pm.get(), 5);
    }
}
