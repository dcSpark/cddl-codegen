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
}
